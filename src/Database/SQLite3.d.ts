type EffectFnCanceller = (
  cancelError: Error,
  onCancelerError: (e: Error) => void,
  onCancelerSuccess: () => void
) => void;

type EffectFnAff<T> = (
  onError: (err: Error) => void,
  onSuccess: (value: T) => void
) => EffectFnCanceller;
