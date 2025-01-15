export const idlFactory = ({ IDL }) => {
  const BlockIndex = IDL.Nat;
  const Tokens = IDL.Nat;
  const Timestamp = IDL.Nat64;
  const TransferError = IDL.Variant({
    'GenericError' : IDL.Record({
      'message' : IDL.Text,
      'error_code' : IDL.Nat,
    }),
    'TemporarilyUnavailable' : IDL.Null,
    'BadBurn' : IDL.Record({ 'min_burn_amount' : Tokens }),
    'Duplicate' : IDL.Record({ 'duplicate_of' : BlockIndex }),
    'BadFee' : IDL.Record({ 'expected_fee' : Tokens }),
    'CreatedInFuture' : IDL.Record({ 'ledger_time' : Timestamp }),
    'TooOld' : IDL.Null,
    'InsufficientFunds' : IDL.Record({ 'balance' : Tokens }),
  });
  const TransferResult = IDL.Variant({
    'Ok' : BlockIndex,
    'Err' : TransferError,
  });
  return IDL.Service({
    'createLeader' : IDL.Func([IDL.Text], [IDL.Nat], []),
    'getAccountIdentifierForUser' : IDL.Func(
        [IDL.Text],
        [IDL.Text],
        ['composite_query'],
      ),
    'getAccountIdentifierFromPrincipal' : IDL.Func(
        [IDL.Text],
        [IDL.Text],
        ['query'],
      ),
    'getLeaders' : IDL.Func([], [IDL.Vec(IDL.Text)], []),
    'getLeadersDiggyBalance' : IDL.Func(
        [],
        [IDL.Vec(IDL.Tuple(IDL.Text, IDL.Nat))],
        [],
      ),
    'getLeadersTotalDiggyBalance' : IDL.Func([], [IDL.Nat], []),
    'getSubaccountBalance' : IDL.Func([IDL.Text], [IDL.Nat], []),
    'getSubaccountOfUserToTransfer' : IDL.Func(
        [IDL.Text],
        [IDL.Vec(IDL.Nat8)],
        ['composite_query'],
      ),
    'getTotalBalance' : IDL.Func([IDL.Nat], [IDL.Nat], []),
    'getUIDS' : IDL.Func([], [IDL.Vec(IDL.Text)], []),
    'removeLeader' : IDL.Func([IDL.Text], [], []),
    'settleBalance' : IDL.Func(
        [IDL.Text, IDL.Text, IDL.Nat],
        [TransferResult],
        [],
      ),
  });
};
export const init = ({ IDL }) => { return []; };
