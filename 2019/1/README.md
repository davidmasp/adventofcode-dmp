# README

## Problem 1

```R
mass = as.numeric(readr::read_lines("1/input.txt"))
fuel = floor(mass/3) - 2
ans = sum(fuel)
clipr::write_clip(ans)
```

## Problem 2

```R
compute_fuel <- function(mass){
    fuel = floor(mass/3) - 2
    if (fuel > 0){
        fuel = fuel + compute_fuel(mass = fuel)
    } else {
        return(0)
    }
    fuel
}
mass = as.numeric(readr::read_lines("1/input2.txt"))
fuel = sapply(mass,compute_fuel)
ans = sum(fuel)
clipr::write_clip(ans)
```
