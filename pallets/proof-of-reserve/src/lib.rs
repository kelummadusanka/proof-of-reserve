#![cfg_attr(not(feature = "std"), no_std)]

pub use pallet::*;

#[frame_support::pallet]
pub mod pallet {
    use frame_support::pallet_prelude::*;
    use frame_system::pallet_prelude::*;
    use frame_support::traits::{Currency, ReservableCurrency, Get};
    use sp_std::vec::Vec;
    use sp_runtime::traits::{Zero, Saturating, Hash};

    #[pallet::pallet]
    #[pallet::without_storage_info]
    pub struct Pallet<T>(_);

    #[pallet::config]
    pub trait Config: frame_system::Config {
        /// The overarching event type.
        type RuntimeEvent: From<Event<Self>> + IsType<<Self as frame_system::Config>::RuntimeEvent>;

        /// The currency trait.
        type Currency: Currency<Self::AccountId> + ReservableCurrency<Self::AccountId>;

        /// Maximum length for proof data (transaction IDs, file hashes, etc.)
        #[pallet::constant]
        type MaxProofDataLength: Get<u32>;

        /// Maximum length for coin name
        #[pallet::constant]
        type MaxCoinNameLength: Get<u32>;

        /// Weight information for extrinsics
        type WeightInfo: WeightInfo;
    }

    /// Information about a reserve entry
    #[derive(Encode, Decode, Clone, PartialEq, Eq, RuntimeDebug, TypeInfo)]
    #[scale_info(skip_type_params(T))]
    pub struct ReserveEntry<T: Config> {
        /// Account that created the reserve
        pub account: T::AccountId,
        /// Proof data (transaction ID, file hash, etc.)
        pub proof: BoundedVec<u8, T::MaxProofDataLength>,
        /// Coin name (e.g., "BTC", "ETH")
        pub coin_name: BoundedVec<u8, T::MaxCoinNameLength>,
        /// Amount of the external coin
        pub coin_amount: u128,
        /// Ratio (coin value / native coin value)
        pub ratio: u128,
        /// Amount of native coins reserved
        pub reserved_amount: BalanceOf<T>,
        /// Block number when reserve was created
        pub created_at: BlockNumberFor<T>,
    }

    type BalanceOf<T> = <<T as Config>::Currency as Currency<<T as frame_system::Config>::AccountId>>::Balance;

    #[pallet::event]
    #[pallet::generate_deposit(pub(super) fn deposit_event)]
    pub enum Event<T: Config> {
        /// Native coins were reserved based on proof
        CoinReserved {
            account: T::AccountId,
            proof_hash: T::Hash,
            coin_name: BoundedVec<u8, T::MaxCoinNameLength>,
            coin_amount: u128,
            ratio: u128,
            reserved_amount: BalanceOf<T>,
        },
        /// Reserved native coins were released
        CoinReleased {
            account: T::AccountId,
            proof_hash: T::Hash,
            coin_name: BoundedVec<u8, T::MaxCoinNameLength>,
            released_amount: BalanceOf<T>,
        },
        /// Reserve entry was updated
        ReserveUpdated {
            account: T::AccountId,
            proof_hash: T::Hash,
            new_reserved_amount: BalanceOf<T>,
        },
    }

    #[pallet::error]
    pub enum Error<T> {
        /// Reserve entry does not exist
        ReserveNotFound,
        /// Only the reserve creator can modify it
        NotReserveOwner,
        /// Proof data too long
        ProofDataTooLong,
        /// Coin name too long
        CoinNameTooLong,
        /// Insufficient funds for reservation
        InsufficientFunds,
        /// Reserve entry already exists for this proof
        ReserveAlreadyExists,
        /// Invalid ratio (cannot be zero)
        InvalidRatio,
        /// Invalid coin amount (cannot be zero)
        InvalidCoinAmount,
        /// Arithmetic overflow in calculation
        ArithmeticOverflow,
        /// Cannot release more than reserved
        InsufficientReserved,
        /// Invalid release amount (cannot be zero)
        InvalidReleaseAmount,
    }

    /// Storage for reserve entries indexed by proof hash
    #[pallet::storage]
    pub type Reserves<T: Config> = StorageMap<
        _,
        Blake2_128Concat,
        T::Hash,
        ReserveEntry<T>
    >;

    /// Map from account to their reserve proof hashes
    #[pallet::storage]
    pub type AccountReserves<T: Config> = StorageMap<
        _,
        Blake2_128Concat,
        T::AccountId,
        BoundedVec<T::Hash, ConstU32<100>>,
        ValueQuery
    >;

    /// Total amount reserved across all entries
    #[pallet::storage]
    #[pallet::getter(fn total_reserved)]
    pub type TotalReserved<T: Config> = StorageValue<_, BalanceOf<T>, ValueQuery>;

    #[pallet::call]
    impl<T: Config> Pallet<T> {
        /// Reserve native coins based on external coin proof
        #[pallet::call_index(0)]
        #[pallet::weight(T::WeightInfo::reserve_coin())]
        pub fn reserve_coin(
            origin: OriginFor<T>,
            proof: Vec<u8>,
            coin_name: Vec<u8>,
            coin_amount: u128,
            ratio: u128,
        ) -> DispatchResult {
            let who = ensure_signed(origin)?;

            // Validate inputs
            ensure!(!proof.is_empty(), Error::<T>::ProofDataTooLong);
            ensure!(!coin_name.is_empty(), Error::<T>::CoinNameTooLong);
            ensure!(coin_amount > 0, Error::<T>::InvalidCoinAmount);
            ensure!(ratio > 0, Error::<T>::InvalidRatio);

            // Create bounded vectors
            let bounded_proof: BoundedVec<u8, T::MaxProofDataLength> = proof.try_into()
                .map_err(|_| Error::<T>::ProofDataTooLong)?;
            let bounded_coin_name: BoundedVec<u8, T::MaxCoinNameLength> = coin_name.try_into()
                .map_err(|_| Error::<T>::CoinNameTooLong)?;

            // Generate proof hash
            let proof_hash = <T::Hashing as Hash>::hash_of(&(&who, &bounded_proof, &bounded_coin_name));

            // Ensure reserve doesn't already exist
            ensure!(!Reserves::<T>::contains_key(&proof_hash), Error::<T>::ReserveAlreadyExists);

            // Calculate reserve amount: coin_amount * ratio
            let reserve_amount_u128 = coin_amount.checked_mul(ratio)
                .ok_or(Error::<T>::ArithmeticOverflow)?;

            // Convert to Balance type
            let reserve_amount: BalanceOf<T> = reserve_amount_u128.try_into()
                .map_err(|_| Error::<T>::ArithmeticOverflow)?;

            // Reserve the native coins
            T::Currency::reserve(&who, reserve_amount)
                .map_err(|_| Error::<T>::InsufficientFunds)?;

            let current_block = <frame_system::Pallet<T>>::block_number();

            // Create reserve entry
            let reserve_entry = ReserveEntry {
                account: who.clone(),
                proof: bounded_proof,
                coin_name: bounded_coin_name.clone(),
                coin_amount,
                ratio,
                reserved_amount: reserve_amount,
                created_at: current_block,
            };

            // Store the reserve
            Reserves::<T>::insert(&proof_hash, &reserve_entry);

            // Update account reserves mapping
            AccountReserves::<T>::try_mutate(&who, |reserves| {
                reserves.try_push(proof_hash)
                    .map_err(|_| Error::<T>::ArithmeticOverflow)
            })?;

            // Update total reserved
            TotalReserved::<T>::mutate(|total| *total = total.saturating_add(reserve_amount));

            Self::deposit_event(Event::CoinReserved {
                account: who,
                proof_hash,
                coin_name: bounded_coin_name,
                coin_amount,
                ratio,
                reserved_amount: reserve_amount,
            });

            Ok(())
        }

        /// Release reserved native coins
        #[pallet::call_index(1)]
        #[pallet::weight(T::WeightInfo::release_coin())]
        pub fn release_coin(
            origin: OriginFor<T>,
            proof: Vec<u8>,
            coin_name: Vec<u8>,
            release_amount: BalanceOf<T>,
        ) -> DispatchResult {
            let who = ensure_signed(origin)?;

            // Validate inputs
            ensure!(!release_amount.is_zero(), Error::<T>::InvalidReleaseAmount);

            // Create bounded vectors for proof hash generation
            let bounded_proof: BoundedVec<u8, T::MaxProofDataLength> = proof.try_into()
                .map_err(|_| Error::<T>::ProofDataTooLong)?;
            let bounded_coin_name: BoundedVec<u8, T::MaxCoinNameLength> = coin_name.try_into()
                .map_err(|_| Error::<T>::CoinNameTooLong)?;

            // Generate proof hash
            let proof_hash = <T::Hashing as Hash>::hash_of(&(&who, &bounded_proof, &bounded_coin_name));

            // Get reserve entry
            let mut reserve_entry = Reserves::<T>::get(&proof_hash)
                .ok_or(Error::<T>::ReserveNotFound)?;

            // Ensure caller is the reserve owner
            ensure!(reserve_entry.account == who, Error::<T>::NotReserveOwner);

            // Ensure we don't release more than reserved
            ensure!(release_amount <= reserve_entry.reserved_amount, Error::<T>::InsufficientReserved);

            // Unreserve the coins
            T::Currency::unreserve(&who, release_amount);

            // Update reserve entry
            reserve_entry.reserved_amount = reserve_entry.reserved_amount.saturating_sub(release_amount);

            // If fully released, remove the reserve entry
            if reserve_entry.reserved_amount.is_zero() {
                Reserves::<T>::remove(&proof_hash);
                
                // Remove from account reserves mapping
                AccountReserves::<T>::mutate(&who, |reserves| {
                    reserves.retain(|&hash| hash != proof_hash);
                });
            } else {
                // Update the reserve entry
                Reserves::<T>::insert(&proof_hash, &reserve_entry);
            }

            // Update total reserved
            TotalReserved::<T>::mutate(|total| *total = total.saturating_sub(release_amount));

            Self::deposit_event(Event::CoinReleased {
                account: who,
                proof_hash,
                coin_name: bounded_coin_name,
                released_amount: release_amount,
            });

            Ok(())
        }

        /// Update an existing reserve with new coin amount and ratio
        #[pallet::call_index(2)]
        #[pallet::weight(T::WeightInfo::update_reserve())]
        pub fn update_reserve(
            origin: OriginFor<T>,
            proof: Vec<u8>,
            coin_name: Vec<u8>,
            new_coin_amount: u128,
            new_ratio: u128,
        ) -> DispatchResult {
            let who = ensure_signed(origin)?;

            // Validate inputs
            ensure!(new_coin_amount > 0, Error::<T>::InvalidCoinAmount);
            ensure!(new_ratio > 0, Error::<T>::InvalidRatio);

            // Create bounded vectors for proof hash generation
            let bounded_proof: BoundedVec<u8, T::MaxProofDataLength> = proof.try_into()
                .map_err(|_| Error::<T>::ProofDataTooLong)?;
            let bounded_coin_name: BoundedVec<u8, T::MaxCoinNameLength> = coin_name.try_into()
                .map_err(|_| Error::<T>::CoinNameTooLong)?;

            // Generate proof hash
            let proof_hash = <T::Hashing as Hash>::hash_of(&(&who, &bounded_proof, &bounded_coin_name));

            // Get reserve entry
            let mut reserve_entry = Reserves::<T>::get(&proof_hash)
                .ok_or(Error::<T>::ReserveNotFound)?;

            // Ensure caller is the reserve owner
            ensure!(reserve_entry.account == who, Error::<T>::NotReserveOwner);

            // Calculate new reserve amount
            let new_reserve_amount_u128 = new_coin_amount.checked_mul(new_ratio)
                .ok_or(Error::<T>::ArithmeticOverflow)?;
            let new_reserve_amount: BalanceOf<T> = new_reserve_amount_u128.try_into()
                .map_err(|_| Error::<T>::ArithmeticOverflow)?;

            let old_reserved = reserve_entry.reserved_amount;

            if new_reserve_amount > old_reserved {
                // Need to reserve more
                let additional_reserve = new_reserve_amount.saturating_sub(old_reserved);
                T::Currency::reserve(&who, additional_reserve)
                    .map_err(|_| Error::<T>::InsufficientFunds)?;
                
                // Update total reserved
                TotalReserved::<T>::mutate(|total| *total = total.saturating_add(additional_reserve));
            } else if new_reserve_amount < old_reserved {
                // Need to unreserve some
                let release_amount = old_reserved.saturating_sub(new_reserve_amount);
                T::Currency::unreserve(&who, release_amount);
                
                // Update total reserved
                TotalReserved::<T>::mutate(|total| *total = total.saturating_sub(release_amount));
            }

            // Update reserve entry
            reserve_entry.coin_amount = new_coin_amount;
            reserve_entry.ratio = new_ratio;
            reserve_entry.reserved_amount = new_reserve_amount;

            Reserves::<T>::insert(&proof_hash, &reserve_entry);

            Self::deposit_event(Event::ReserveUpdated {
                account: who,
                proof_hash,
                new_reserved_amount: new_reserve_amount,
            });

            Ok(())
        }
    }

    // Helper functions
    impl<T: Config> Pallet<T> {
        /// Get reserve entry by proof hash
        pub fn get_reserve(proof_hash: &T::Hash) -> Option<ReserveEntry<T>> {
            Reserves::<T>::get(proof_hash)
        }

        /// Get all reserves for an account
        pub fn get_account_reserves(account: &T::AccountId) -> Vec<T::Hash> {
            AccountReserves::<T>::get(account).into_inner()
        }

        /// Get total reserved amount for an account
        pub fn get_account_reserved_total(account: &T::AccountId) -> BalanceOf<T> {
            let reserve_hashes = Self::get_account_reserves(account);
            let mut total: BalanceOf<T> = Zero::zero();
            
            for hash in reserve_hashes {
                if let Some(reserve) = Self::get_reserve(&hash) {
                    total = total.saturating_add(reserve.reserved_amount);
                }
            }
            
            total
        }

        /// Generate proof hash from components
        pub fn generate_proof_hash(
            account: &T::AccountId,
            proof: &[u8],
            coin_name: &[u8],
        ) -> T::Hash {
            <T::Hashing as Hash>::hash_of(&(account, proof, coin_name))
        }
    }

    /// Weight functions (implement based on benchmarks)
    pub trait WeightInfo {
        fn reserve_coin() -> Weight;
        fn release_coin() -> Weight;
        fn update_reserve() -> Weight;
    }

    /// Default weights implementation
    impl WeightInfo for () {
        fn reserve_coin() -> Weight {
            Weight::from_parts(50_000_000, 0)
                .saturating_add(Weight::from_parts(0, 1024))
        }
        fn release_coin() -> Weight {
            Weight::from_parts(40_000_000, 0)
                .saturating_add(Weight::from_parts(0, 512))
        }
        fn update_reserve() -> Weight {
            Weight::from_parts(45_000_000, 0)
                .saturating_add(Weight::from_parts(0, 768))
        }
    }
}