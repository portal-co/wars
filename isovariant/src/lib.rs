pub trait Iso<T: Into<Self>>: Into<T> {}
impl<T: Into<U>, U: Into<T>> Iso<U> for T {}
pub trait TryIso<T: TryInto<Self, Error: Iso<Self::Error>>>:
    TryInto<T, Error: Iso<T::Error>>
{
}
impl<T: TryInto<U, Error: Iso<U::Error>>, U: TryInto<T, Error: Iso<T::Error>>> TryIso<T> for U {}
