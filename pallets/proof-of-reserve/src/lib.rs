#![cfg_attr(not(feature = "std"), no_std)]

pub use pallet::*;

#[frame_support::pallet]
pub mod pallet {
    use frame_support::traits::ExistenceRequirement;
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

        /// Maximum number of assets that can be tracked in a reserve proof
        #[pallet::constant]
        type MaxAssetsPerProof: Get<u32>;

        /// Maximum length for external proof data (signatures, merkle proofs, etc.)
        #[pallet::constant]
        type MaxProofDataLength: Get<u32>;

        /// Maximum length for asset identifier
        #[pallet::constant]
        type MaxAssetIdLength: Get<u32>;

        /// Weight information for extrinsics
        type WeightInfo: WeightInfo;
    }

    /// Information about a specific asset in a reserve proof
    /// We use a tuple-based approach: (asset_id, amount, last_updated)
    pub type AssetReserve<Balance> = (
        BoundedVec<u8, ConstU32<32>>, // asset_id
        Balance,                      // amount
        u32,                         // last_updated
    );

    /// Helper functions for working with AssetReserve tuples
    pub struct AssetReserveHelper;
    
    impl AssetReserveHelper {
        pub fn new<Balance>(
            asset_id: BoundedVec<u8, ConstU32<32>>, 
            amount: Balance, 
            last_updated: u32
        ) -> AssetReserve<Balance> {
            (asset_id, amount, last_updated)
        }
        
        pub fn asset_id<Balance>(asset: &AssetReserve<Balance>) -> &BoundedVec<u8, ConstU32<32>> {
            &asset.0
        }
        
        pub fn amount<Balance>(asset: &AssetReserve<Balance>) -> &Balance {
            &asset.1
        }
        
        pub fn last_updated<Balance>(asset: &AssetReserve<Balance>) -> u32 {
            asset.2
        }
    }

    /// Complete reserve proof information
    #[derive(Encode, Decode, Clone, PartialEq, Eq, RuntimeDebug, TypeInfo)]
    #[scale_info(skip_type_params(AccountId, Balance, BlockNumber, BoundedAssets, BoundedProofData))]
    pub struct ReserveProof<AccountId, Balance, BlockNumber, BoundedAssets, BoundedProofData> {
        /// Account submitting the proof
        pub prover: AccountId,
        /// Total value in native currency
        pub total_value: Balance,
        /// List of assets and their amounts
        pub assets: BoundedAssets,
        /// External proof data (signatures, merkle proofs, etc.)
        pub proof_data: BoundedProofData,
        /// Block number when proof was submitted
        pub block_number: BlockNumber,
        /// Expiry block number
        pub expiry_block: BlockNumber,
        /// Whether this proof has been verified
        pub is_verified: bool,
    }

    type BalanceOf<T> = <<T as Config>::Currency as Currency<<T as frame_system::Config>::AccountId>>::Balance;
    type BoundedAssets<T> = BoundedVec<AssetReserve<BalanceOf<T>>, <T as Config>::MaxAssetsPerProof>;
    type BoundedProofData<T> = BoundedVec<u8, <T as Config>::MaxProofDataLength>;

    #[pallet::event]
    #[pallet::generate_deposit(pub(super) fn deposit_event)]
    pub enum Event<T: Config> {
        /// A reserve proof was submitted
        ReserveProofSubmitted {
            proof_id: T::Hash,
            prover: T::AccountId,
            total_value: BalanceOf<T>,
            expiry_block: BlockNumberFor<T>,
        },
        /// A reserve proof was verified
        ReserveProofVerified {
            proof_id: T::Hash,
            verifier: T::AccountId,
        },
        /// A reserve proof was challenged
        ReserveProofChallenged {
            proof_id: T::Hash,
            challenger: T::AccountId,
            reason: BoundedVec<u8, ConstU32<256>>,
        },
        /// A reserve proof expired
        ReserveProofExpired {
            proof_id: T::Hash,
        },
        /// Asset reserves were updated
        AssetReservesUpdated {
            proof_id: T::Hash,
            prover: T::AccountId,
            asset_count: u32,
        },

           /// Pegged tokens were minted
    PeggedMinted {
        who: T::AccountId,
        amount: BalanceOf<T>,
    },
    }

    #[pallet::error]
    pub enum Error<T> {
        /// Reserve proof does not exist
        ProofNotFound,
        /// Only the prover can update their proof
        NotProofOwner,
        /// Proof has already expired
        ProofExpired,
        /// Proof is already verified
        AlreadyVerified,
        /// Invalid asset data
        InvalidAssetData,
        /// Proof data too long
        ProofDataTooLong,
        /// Asset list too long
        TooManyAssets,
        /// Insufficient funds for bond
        InsufficientFunds,
        /// Cannot verify own proof
        CannotVerifyOwnProof,
        /// Proof is not yet expired
        ProofNotExpired,

        CustodyNotSet, 
    }

    /// Storage for reserve proofs indexed by hash
    #[pallet::storage]
    pub type ReserveProofs<T: Config> = StorageMap<
        _,
        Blake2_128Concat,
        T::Hash,
        ReserveProof<
            T::AccountId,
            BalanceOf<T>,
            BlockNumberFor<T>,
            BoundedAssets<T>,
            BoundedProofData<T>
        >
    >;

    /// Total supply of pegged tokens
#[pallet::storage]
#[pallet::getter(fn pegged_supply)]
pub type PeggedSupply<T: Config> = StorageValue<_, BalanceOf<T>, ValueQuery>;

#[pallet::storage]
#[pallet::getter(fn pegged_balances)]
/// Pegged token balances per user
pub type PeggedBalances<T: Config> = StorageMap<
    _,
    Blake2_128Concat,
    T::AccountId,
    BalanceOf<T>,
    ValueQuery
>;

    /// Map from account to their active proof IDs
    #[pallet::storage]
    pub type AccountProofs<T: Config> = StorageMap<
        _,
        Blake2_128Concat,
        T::AccountId,
        BoundedVec<T::Hash, ConstU32<10>>,
        ValueQuery
    >;
#[pallet::storage]
#[pallet::getter(fn custody_account)]
/// The account that holds custody of deposited native tokens
pub type CustodyAccount<T: Config> = StorageValue<_, T::AccountId>;

    /// Counter for generating unique proof IDs
    #[pallet::storage]
    pub type ProofCounter<T: Config> = StorageValue<_, u64, ValueQuery>;

    #[pallet::call]
    impl<T: Config> Pallet<T> {

               /// Deposit native currency → mint pegged token
        #[pallet::call_index(7)]
        #[pallet::weight(10_000)]
        pub fn deposit_and_mint(origin: OriginFor<T>, amount: BalanceOf<T>) -> DispatchResult {
            let who = ensure_signed(origin)?;
            let custody = CustodyAccount::<T>::get().ok_or(Error::<T>::CustodyNotSet)?;

            // transfer funds into custody
            T::Currency::transfer(&who, &custody, amount, ExistenceRequirement::KeepAlive)
                .map_err(|_| Error::<T>::InsufficientFunds)?;

            // mint pegged token
            PeggedBalances::<T>::mutate(&who, |bal| *bal += amount);
            PeggedSupply::<T>::mutate(|supply| *supply += amount);

            Self::deposit_event(Event::PeggedMinted { who, amount });
            Ok(())
        }

        /// Submit a new reserve proof
        #[pallet::call_index(0)]
        #[pallet::weight(T::WeightInfo::submit_proof())]
        pub fn submit_proof(
            origin: OriginFor<T>,
            assets: Vec<AssetReserve<BalanceOf<T>>>,
            proof_data: Vec<u8>,
            validity_period: BlockNumberFor<T>,
        ) -> DispatchResult {
            let who = ensure_signed(origin)?;

            // Validate input lengths
            ensure!(
                assets.len() <= T::MaxAssetsPerProof::get() as usize,
                Error::<T>::TooManyAssets
            );
            ensure!(
                proof_data.len() <= T::MaxProofDataLength::get() as usize,
                Error::<T>::ProofDataTooLong
            );

            let current_block = <frame_system::Pallet<T>>::block_number();
            let expiry_block = current_block.saturating_add(validity_period);

            // Calculate total value (simplified - in practice you'd use price oracles)
            let total_value = assets.iter()
                .map(|asset| asset.1) // asset.amount (second element of tuple)
                .fold(Zero::zero(), |acc: BalanceOf<T>, amount| acc.saturating_add(amount));

            // Generate unique proof ID
            let proof_counter = ProofCounter::<T>::get();
            ProofCounter::<T>::put(proof_counter.saturating_add(1));
            
            let proof_id = T::Hashing::hash_of(&(&who, proof_counter, current_block));

            // Create bounded vectors
            let bounded_assets: BoundedAssets<T> = assets.try_into()
                .map_err(|_| Error::<T>::TooManyAssets)?;
            let bounded_proof_data: BoundedProofData<T> = proof_data.try_into()
                .map_err(|_| Error::<T>::ProofDataTooLong)?;

            // Create the proof
            let proof = ReserveProof {
                prover: who.clone(),
                total_value,
                assets: bounded_assets,
                proof_data: bounded_proof_data,
                block_number: current_block,
                expiry_block,
                is_verified: false,
            };

            // Store the proof
            ReserveProofs::<T>::insert(&proof_id, &proof);

            // Update account proofs mapping
            AccountProofs::<T>::try_mutate(&who, |proofs| {
                proofs.try_push(proof_id)
                    .map_err(|_| Error::<T>::TooManyAssets)
            })?;

            Self::deposit_event(Event::ReserveProofSubmitted {
                proof_id,
                prover: who,
                total_value,
                expiry_block,
            });

            Ok(())
        }

        /// Verify a reserve proof (typically done by auditors or validators)
        #[pallet::call_index(1)]
        #[pallet::weight(T::WeightInfo::verify_proof())]
        pub fn verify_proof(
            origin: OriginFor<T>,
            proof_id: T::Hash,
        ) -> DispatchResult {
            let who = ensure_signed(origin)?;

            let mut proof = ReserveProofs::<T>::get(&proof_id)
                .ok_or(Error::<T>::ProofNotFound)?;

            // Ensure verifier is not the prover
            ensure!(proof.prover != who, Error::<T>::CannotVerifyOwnProof);

            let current_block = <frame_system::Pallet<T>>::block_number();
            ensure!(current_block <= proof.expiry_block, Error::<T>::ProofExpired);
            ensure!(!proof.is_verified, Error::<T>::AlreadyVerified);

            // Mark as verified
            proof.is_verified = true;
            ReserveProofs::<T>::insert(&proof_id, &proof);

            Self::deposit_event(Event::ReserveProofVerified {
                proof_id,
                verifier: who,
            });

            Ok(())
        }

        /// Challenge a reserve proof
        #[pallet::call_index(2)]
        #[pallet::weight(T::WeightInfo::challenge_proof())]
        pub fn challenge_proof(
            origin: OriginFor<T>,
            proof_id: T::Hash,
            reason: Vec<u8>,
        ) -> DispatchResult {
            let who = ensure_signed(origin)?;

            let proof = ReserveProofs::<T>::get(&proof_id)
                .ok_or(Error::<T>::ProofNotFound)?;

            let current_block = <frame_system::Pallet<T>>::block_number();
            ensure!(current_block <= proof.expiry_block, Error::<T>::ProofExpired);

            let bounded_reason: BoundedVec<u8, ConstU32<256>> = reason.try_into()
                .map_err(|_| Error::<T>::ProofDataTooLong)?;

            Self::deposit_event(Event::ReserveProofChallenged {
                proof_id,
                challenger: who,
                reason: bounded_reason,
            });

            Ok(())
        }

        /// Update asset reserves in an existing proof
        #[pallet::call_index(3)]
        #[pallet::weight(T::WeightInfo::update_assets())]
        pub fn update_assets(
            origin: OriginFor<T>,
            proof_id: T::Hash,
            assets: Vec<AssetReserve<BalanceOf<T>>>,
        ) -> DispatchResult {
            let who = ensure_signed(origin)?;

            let mut proof = ReserveProofs::<T>::get(&proof_id)
                .ok_or(Error::<T>::ProofNotFound)?;

            ensure!(proof.prover == who, Error::<T>::NotProofOwner);

            let current_block = <frame_system::Pallet<T>>::block_number();
            ensure!(current_block <= proof.expiry_block, Error::<T>::ProofExpired);

            // Validate input
            ensure!(
                assets.len() <= T::MaxAssetsPerProof::get() as usize,
                Error::<T>::TooManyAssets
            );

            // Update assets
            let bounded_assets: BoundedAssets<T> = assets.try_into()
                .map_err(|_| Error::<T>::TooManyAssets)?;

            let total_value = bounded_assets.iter()
                .map(|asset| asset.1) // asset.amount (second element of tuple)
                .fold(Zero::zero(), |acc: BalanceOf<T>, amount| acc.saturating_add(amount));

            proof.assets = bounded_assets.clone();
            proof.total_value = total_value;
            proof.is_verified = false; // Reset verification status

            ReserveProofs::<T>::insert(&proof_id, &proof);

            Self::deposit_event(Event::AssetReservesUpdated {
                proof_id,
                prover: who,
                asset_count: bounded_assets.len() as u32,
            });

            Ok(())
        }

        /// Remove expired proofs (can be called by anyone for cleanup)
        #[pallet::call_index(4)]
        #[pallet::weight(T::WeightInfo::cleanup_expired_proof())]
        pub fn cleanup_expired_proof(
            origin: OriginFor<T>,
            proof_id: T::Hash,
        ) -> DispatchResult {
            let _who = ensure_signed(origin)?;

            let proof = ReserveProofs::<T>::get(&proof_id)
                .ok_or(Error::<T>::ProofNotFound)?;

            let current_block = <frame_system::Pallet<T>>::block_number();
            ensure!(current_block > proof.expiry_block, Error::<T>::ProofNotExpired);

            // Remove from storage
            ReserveProofs::<T>::remove(&proof_id);

            // Remove from account proofs mapping
            AccountProofs::<T>::mutate(&proof.prover, |proofs| {
                proofs.retain(|&id| id != proof_id);
            });

            Self::deposit_event(Event::ReserveProofExpired { proof_id });

            Ok(())
        }
    }

    // Helper functions
    impl<T: Config> Pallet<T> {
        /// Get proof by ID
        pub fn get_proof(proof_id: &T::Hash) -> Option<ReserveProof<
            T::AccountId,
            BalanceOf<T>,
            BlockNumberFor<T>,
            BoundedAssets<T>,
            BoundedProofData<T>
        >> {
            ReserveProofs::<T>::get(proof_id)
        }

        /// Get all proofs for an account
        pub fn get_account_proofs(account: &T::AccountId) -> Vec<T::Hash> {
            AccountProofs::<T>::get(account).into_inner()
        }

        /// Check if proof exists and is valid
        pub fn is_proof_valid(proof_id: &T::Hash) -> bool {
            if let Some(proof) = Self::get_proof(proof_id) {
                let current_block = <frame_system::Pallet<T>>::block_number();
                current_block <= proof.expiry_block
            } else {
                false
            }
        }

        /// Get proof total value
        pub fn get_proof_value(proof_id: &T::Hash) -> Option<BalanceOf<T>> {
            Self::get_proof(proof_id).map(|proof| proof.total_value)
        }
    }

    /// Weight functions (implement based on benchmarks)
    pub trait WeightInfo {
        fn submit_proof() -> Weight;
        fn verify_proof() -> Weight;
        fn challenge_proof() -> Weight;
        fn update_assets() -> Weight;
        fn cleanup_expired_proof() -> Weight;
    }

    /// Default weights implementation
    impl WeightInfo for () {
        fn submit_proof() -> Weight {
            Weight::from_parts(50_000_000, 0)
                .saturating_add(Weight::from_parts(0, 1024))
        }
        fn verify_proof() -> Weight {
            Weight::from_parts(25_000_000, 0)
                .saturating_add(Weight::from_parts(0, 512))
        }
        fn challenge_proof() -> Weight {
            Weight::from_parts(20_000_000, 0)
        }
        fn update_assets() -> Weight {
            Weight::from_parts(40_000_000, 0)
                .saturating_add(Weight::from_parts(0, 1024))
        }
        fn cleanup_expired_proof() -> Weight {
            Weight::from_parts(15_000_000, 0)
        }
    }
}



