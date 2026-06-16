# NH3 volatilization emission factor correction notes

Source file: `NH3_EF_correction.pptx`

## Overall purpose

This slide deck explains how NH3 volatilization can be represented with a simple temperature corrected emission factor. The logic moves from surface NH3 chemistry, to NH3 flux, to a practical correction equation:

$$
EF_T = EF_{Tref} \cdot C_T^{T - Tref}
$$

The main message is that NH3 volatilization is controlled by surface NH3 concentration, atmospheric transport resistance, temperature, pH, and total ammoniacal nitrogen. The final figure tests whether a generalized temperature response can reproduce modelled mean emission factors across animal categories.

## Slide 1. Surface NH3 concentration and equilibrium ratio

### Equation 1

$$
[NH_3]_{srf} = \frac{K_w}{K_{ha}K_b} \cdot \Gamma
$$

### Meaning

This equation estimates the NH3 concentration at the emitting surface.

`[NH3]srf` means NH3 concentration at the surface.

`Kw` is the water dissociation equilibrium constant.

`Kha` is related to Henry's law partitioning between aqueous NH3 and gaseous NH3.

`Kb` is the base dissociation equilibrium constant for ammonia.

`Gamma` is the ammonium to hydrogen ion ratio.

The term

$$
\frac{K_w}{K_{ha}K_b}
$$

summarizes the chemical equilibrium control. It determines how easily ammonium nitrogen can exist as volatile NH3 under a given chemical environment.

### Equation 2

$$
\Gamma = \frac{[NH_4^+]}{[H^+]}
$$

### Meaning

This equation defines the equilibrium ratio used in Equation 1.

`[NH4+]` is ammonium concentration.

`[H+]` is hydrogen ion concentration.

Because pH is inversely related to `[H+]`, higher pH means lower `[H+]`. Therefore, higher pH increases `Gamma`, which increases surface NH3 concentration and volatilization potential.

In words:

$$
higher\ pH \Rightarrow lower\ [H^+] \Rightarrow larger\ \Gamma \Rightarrow more\ NH_3\ volatilization
$$

The slide also shows a resistance symbol, `R*`, representing the resistance pathway for NH3 movement away from the emitting surface.

## Slide 2. NH3 flux and simplified EF correction

### Equation 3

$$
F_{NH_3} = \frac{[NH_3]_{srf} - [NH_3]_{atm}}{R_{atm}}
$$

### Meaning

This is a resistance based flux equation.

`FNH3` is the NH3 volatilization flux.

`[NH3]srf` is NH3 concentration at the emitting surface.

`[NH3]atm` is NH3 concentration in the atmosphere.

`Ratm` is atmospheric resistance.

The numerator is the concentration gradient between the emitting surface and the surrounding air. The denominator is the resistance to transport. A larger surface to air concentration difference increases flux. A larger atmospheric resistance decreases flux.

In words:

$$
NH_3\ flux = \frac{surface\ concentration - atmospheric\ concentration}{atmospheric\ resistance}
$$

### Equation 4

$$
[NH_3]_{srf} = \frac{161500}{T}\exp\left(-\frac{10378}{T}\right)\frac{[NH_4^+]}{[H^+]}
$$

### Meaning

This equation expands the surface NH3 concentration using a temperature dependent equilibrium term and the ammonium to hydrogen ion ratio.

`T` is temperature, most likely in Kelvin because it is used in an exponential thermodynamic expression.

The term

$$
\frac{161500}{T}\exp\left(-\frac{10378}{T}\right)
$$

represents the temperature effect on the NH3 equilibrium.

The term

$$
\frac{[NH_4^+]}{[H^+]}
$$

represents the ammonium and pH effect.

This equation shows that surface NH3 increases when temperature increases, ammonium increases, or pH increases.

### Equation 5

$$
F_{NH_3} = f(T) \cdot f(pH) \cdot TAN
$$

### Meaning

This is a simplified practical version of the NH3 flux model.

`f(T)` is the temperature response function.

`f(pH)` is the pH response function.

`TAN` means total ammoniacal nitrogen, usually defined as:

$$
TAN = NH_3 + NH_4^+
$$

The equation says that NH3 volatilization can be approximated as a function of temperature, pH, and available ammoniacal nitrogen.

This is the conceptual bridge from a mechanistic chemistry model to a practical emission factor correction.

### Equation 6

$$
EF_T = EF_{Tref} \cdot C_T^{T - Tref}
$$

### Meaning

This is the main emission factor correction equation.

`EFT` is the NH3 emission factor at temperature `T`.

`EFTref` is the NH3 emission factor at the reference temperature `Tref`.

`CT` is the temperature correction coefficient.

`T minus Tref` is the temperature difference from the reference temperature.

If `CT = 1.05`, then each 1 degree increase above the reference temperature increases the emission factor by about 5 percent. If the temperature is below the reference temperature, the exponent is negative and the emission factor decreases.

Important note: the slide does not explicitly state the numerical value of `Tref` or `EFTref`. From the final figure, `Tref = 15 degrees C` appears to be a reasonable inferred reference temperature, but this should be stated explicitly in the method if used.

## Slide 3. Q10 formulation and connection to the EF correction

### Equation 7

$$
F_{NH_3} = f(T) \cdot f(pH) \cdot TAN
$$

### Meaning

This repeated equation reminds us that the flux model is being simplified around major controlling factors: temperature, pH, and total ammoniacal nitrogen.

### Equation 8

$$
Q_{10} = \left(\frac{F_1}{F_2}\right)^{\frac{10}{T_1 - T_2}}
$$

### Meaning

This equation defines the Q10 value from two fluxes at two temperatures.

`Q10` is the factor by which flux changes for a 10 degree temperature change.

`F1` is the flux at temperature `T1`.

`F2` is the flux at temperature `T2`.

The exponent standardizes the flux ratio to a 10 degree temperature interval.

This equation is equivalent to the more common form:

$$
Q_{10} = \left(\frac{F_2}{F_1}\right)^{\frac{10}{T_2 - T_1}}
$$

Both forms give the same result if the flux and temperature order are kept consistent.

### Equation 9

$$
F_2 = F_1 \cdot Q_{10}^{\frac{T_2 - T_1}{10}}
$$

### Meaning

This equation predicts the flux at a second temperature using Q10.

Starting from the common Q10 expression:

$$
Q_{10} = \left(\frac{F_2}{F_1}\right)^{\frac{10}{T_2 - T_1}}
$$

Raise both sides to the power:

$$
\frac{T_2 - T_1}{10}
$$

Then:

$$
Q_{10}^{\frac{T_2 - T_1}{10}} = \frac{F_2}{F_1}
$$

Multiplying both sides by `F1` gives:

$$
F_2 = F_1 \cdot Q_{10}^{\frac{T_2 - T_1}{10}}
$$

This means the flux is multiplied by the Q10 factor according to how many 10 degree intervals separate `T1` and `T2`.

### Equation 10

$$
EF_T = EF_{Tref} \cdot C_T^{T - Tref}
$$

### Meaning

This is the same temperature correction idea as the Q10 equation, but expressed as a per degree correction rather than a per 10 degree correction.

The relationship between `CT` and `Q10` is:

$$
C_T = Q_{10}^{1/10}
$$

and therefore:

$$
Q_{10} = C_T^{10}
$$

So `CT` is the multiplier per 1 degree temperature change, while `Q10` is the multiplier per 10 degree temperature change.

## Slide 4. Comparison of modelled mean EF and generalized mean EF

The final slide shows six panels. Each panel compares the modelled mean NH3 emission factor with the generalized mean NH3 emission factor across annual mean temperature.

The y axis is:

$$
NH_3\ emission\ factor\ (\%)
$$

The x axis is:

$$
Annual\ mean\ temperature\ (degree\ C)
$$

The blue line represents modelled mean EF.

The orange line represents generalized mean EF.

Each animal category has its own fitted temperature correction coefficient, `CT`.

## Reported CT values from the figure

| Animal category | CT | Approximate increase per 1 degree C | Implied Q10, CT^10 |
|---|---:|---:|---:|
| DAIRY_CATTLE | 1.056 | 5.6 percent | 1.724 |
| BEEF_CATTLE | 1.065 | 6.5 percent | 1.877 |
| OTHER_CATTLE | 1.062 | 6.2 percent | 1.825 |
| BUFFALO_BEEF | 1.067 | 6.7 percent | 1.913 |
| SHEEP | 1.049 | 4.9 percent | 1.613 |
| PIG | 1.036 | 3.6 percent | 1.424 |

## Approximate inferred EFTref values if Tref is 15 degrees C

The slide does not state `Tref` or `EFTref` explicitly. If `Tref = 15 degrees C` is assumed based on the apparent anchor point of the generalized curves, approximate `EF15` values are:

| Animal category | Approximate EF at 15 degrees C |
|---|---:|
| DAIRY_CATTLE | 12.2 percent |
| BEEF_CATTLE | 10.0 percent |
| OTHER_CATTLE | 7.6 percent |
| BUFFALO_BEEF | 13.4 percent |
| SHEEP | 25.0 percent |
| PIG | 29.6 percent |

These values are inferred visually from the plotted generalized curves and should be treated as approximate. They should not be presented as exact values unless they are confirmed from the underlying model output.

## Example use of the correction equation

Using dairy cattle as an example, if `Tref = 15 degrees C`, `EF15 = 12.2 percent`, and `CT = 1.056`, then:

$$
EF_T = 12.2 \cdot 1.056^{T - 15}
$$

For pig, if `Tref = 15 degrees C`, `EF15 = 29.6 percent`, and `CT = 1.036`, then:

$$
EF_T = 29.6 \cdot 1.036^{T - 15}
$$

## Key interpretation

The slides show that NH3 volatilization can be described mechanistically through surface NH3 concentration and atmospheric resistance, but for emission factor correction it can be simplified to a temperature response equation.

The practical correction is:

$$
EF_T = EF_{Tref} \cdot C_T^{T - Tref}
$$

The coefficient `CT` controls how strongly the emission factor increases with temperature. The final plot suggests that a single exponential temperature correction can reproduce the general pattern of the modelled mean EF for each animal category, although some categories show deviations at warmer temperatures.

## App extension for manure management assumptions

The Shiny app now keeps the temperature correction as the base EF and then applies manure-management adjustments:

$$
EF_{adjusted} = EF_T \cdot M_{housing} \cdot M_{storage,cover}
$$

`M_housing` is an editable housing/source multiplier. It defaults to `1.0` for every housing type because the local reference files do not provide clean housing-specific NH3 default factors.

`M_storage,cover` is based on the 2019 IPCC Refinement Table 10.22 nitrogen-loss fractions for volatilisation of NH3 and NOx from manure management systems. The app normalizes each available `FracGas_MS` value against the selected animal group's liquid/slurry without natural crust value.

The app maps animal types to IPCC Table 10.22 groups as follows:

| App animal type | IPCC group |
|---|---|
| DAIRY_CATTLE | Dairy Cow |
| BEEF_CATTLE | Other Cattle |
| OTHER_CATTLE | Other Cattle |
| BUFFALO_BEEF | Other Cattle |
| SHEEP | Other animals |
| PIG | Swine |

Emptying/removal is handled as a storage mass-flow assumption rather than an EF multiplier. At the start of each selected removal month, the app removes the selected percentage of TAN already stored, then adds the current month's generated TAN. Removed TAN leaves this storage model and is not counted as later field-application NH3 emission.

## Suggested clarification for the slide deck

The deck should explicitly state the reference temperature `Tref` and the corresponding reference emission factor `EFTref` for each animal category. Without these values, the correction equation cannot be applied uniquely.

A useful additional table would be:

| Animal category | Tref | EFTref | CT |
|---|---:|---:|---:|
| DAIRY_CATTLE | 15 degrees C, if confirmed | EF at 15 degrees C | 1.056 |
| BEEF_CATTLE | 15 degrees C, if confirmed | EF at 15 degrees C | 1.065 |
| OTHER_CATTLE | 15 degrees C, if confirmed | EF at 15 degrees C | 1.062 |
| BUFFALO_BEEF | 15 degrees C, if confirmed | EF at 15 degrees C | 1.067 |
| SHEEP | 15 degrees C, if confirmed | EF at 15 degrees C | 1.049 |
| PIG | 15 degrees C, if confirmed | EF at 15 degrees C | 1.036 |
