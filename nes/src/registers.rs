macro_rules! reg_from_impl {
    ($to:ty, $from:ty) => {
        reg_from_impl!($to, $from, u8);
    };

    ($to:ty, $from:ty, $cast:ty) => {
        impl From<$from> for $to {
            fn from(value: $from) -> Self {
                Self(value as $cast)
            }
        }

        impl From<$to> for $from {
            fn from(value: $to) -> Self {
                value.0 as $from
            }
        }
    };
}

macro_rules! reg_add_impl {
    ($cls:ty, $plus:ty) => {
        impl Add<$plus> for $cls {
            type Output = $plus;
            fn add(self, rhs: $plus) -> Self::Output {
                self.0 as $plus + rhs
            }
        }

        impl Add<$cls> for $plus {
            type Output = $plus;
            fn add(self, rhs: $cls) -> Self::Output {
                self + rhs.0 as $plus
            }
        }
    };
}

macro_rules! bit_or {
    ($cls:ty, $v:ty) => {
        impl BitOr for $cls {
            type Output = $cls;
            fn bitor(self, rhs: Self) -> Self::Output {
                Self(self.0 | rhs.0)
            }
        }

        impl BitOrAssign for $cls {
            fn bitor_assign(&mut self, rhs: Self) {
                self.0 |= rhs.0
            }
        }

        impl BitOr<$v> for $cls {
            type Output = $cls;
            fn bitor(self, rhs: $v) -> Self::Output {
                Self(self.0 | rhs)
            }
        }

        impl BitOrAssign<$v> for $cls {
            fn bitor_assign(&mut self, rhs: $v) {
                self.0 |= rhs
            }
        }
    };
}

macro_rules! bit_and {
    ($cls:ty, $v:ty) => {
        impl BitAnd for $cls {
            type Output = $cls;
            fn bitand(self, rhs: Self) -> Self::Output {
                Self(self.0 & rhs.0)
            }
        }

        impl BitAndAssign for $cls {
            fn bitand_assign(&mut self, rhs: Self) {
                self.0 &= rhs.0
            }
        }

        impl BitAnd<$v> for $cls {
            type Output = $cls;
            fn bitand(self, rhs: $v) -> Self::Output {
                Self(self.0 & rhs)
            }
        }

        impl BitAndAssign<$v> for $cls {
            fn bitand_assign(&mut self, rhs: $v) {
                self.0 &= rhs
            }
        }
    };
}

macro_rules! bit_xor {
    ($cls:ty, $v:ty) => {
        impl BitXor for $cls {
            type Output = $cls;
            fn bitxor(self, rhs: Self) -> Self::Output {
                Self(self.0 ^ rhs.0)
            }
        }

        impl BitXorAssign for $cls {
            fn bitxor_assign(&mut self, rhs: Self) {
                self.0 ^= rhs.0
            }
        }

        impl BitXor<$v> for $cls {
            type Output = $cls;
            fn bitxor(self, rhs: $v) -> Self::Output {
                Self(self.0 ^ rhs)
            }
        }

        impl BitXorAssign<$v> for $cls {
            fn bitxor_assign(&mut self, rhs: $v) {
                self.0 ^= rhs
            }
        }
    };
}

macro_rules! not {
    ($cls:ty) => {
        impl Not for $cls {
            type Output = $cls;
            fn not(self) -> Self::Output {
                Self(!self.0)
            }
        }
    };
}

macro_rules! partial_eq {
    ($cls:ty, $with:ty) => {
        impl PartialEq<$with> for $cls {
            fn eq(&self, other: &$with) -> bool {
                &self.0 == other
            }
        }
    };
}

macro_rules! display {
    ($cls:ty [$a:tt, $b:tt, $c:tt, $d:tt, $e:tt, $f:tt, $g:tt, $h:tt]) => {
        impl fmt::Display for $cls {
            fn fmt(&self, fout: &mut fmt::Formatter<'_>) -> fmt::Result {
                let func = |f, i, e| match self.get(f) == 1 {
                    true => i,
                    false => e,
                };

                let a = func(<$cls>::$a, stringify!($a), ".");
                let b = func(<$cls>::$b, stringify!($b), ".");
                let c = func(<$cls>::$c, stringify!($c), ".");
                let d = func(<$cls>::$d, stringify!($d), ".");
                let e = func(<$cls>::$e, stringify!($e), ".");
                let f = func(<$cls>::$f, stringify!($f), ".");
                let g = func(<$cls>::$g, stringify!($g), ".");
                let h = func(<$cls>::$h, stringify!($h), ".");

                write!(
                    fout,
                    "{:?}{:?}{:?}{:?}{:?}{:?}{:?}{:?}",
                    a, b, c, d, e, f, g, h
                )
            }
        }
    };

    ($cls:ty [$a:tt, $b:tt, $c:tt, $d:tt, $e:tt, $f:tt, $g:tt, $h:tt], [$aa:tt, $bb:tt, $cc:tt, $dd:tt, $ee:tt, $ff:tt, $gg:tt, $hh:tt]) => {
        impl fmt::Display for $cls {
            fn fmt(&self, fout: &mut fmt::Formatter<'_>) -> fmt::Result {
                let func = |f, i, e| match self.get(f) == 1 {
                    true => i,
                    false => e,
                };

                let a = func(<$cls>::$a, stringify!($aa), ".");
                let b = func(<$cls>::$b, stringify!($bb), ".");
                let c = func(<$cls>::$c, stringify!($cc), ".");
                let d = func(<$cls>::$d, stringify!($dd), ".");
                let e = func(<$cls>::$e, stringify!($ee), ".");
                let f = func(<$cls>::$f, stringify!($ff), ".");
                let g = func(<$cls>::$g, stringify!($gg), ".");
                let h = func(<$cls>::$h, stringify!($hh), ".");

                write!(
                    fout,
                    "{:?}{:?}{:?}{:?}{:?}{:?}{:?}{:?}",
                    a, b, c, d, e, f, g, h
                )
            }
        }
    };
}

pub(crate) use reg_from_impl;
pub(crate) use reg_add_impl;
pub(crate) use bit_or;
pub(crate) use bit_and;
pub(crate) use bit_xor;
pub(crate) use not;
pub(crate) use partial_eq;
pub(crate) use display;
