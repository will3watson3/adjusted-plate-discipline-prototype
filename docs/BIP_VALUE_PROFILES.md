# BIP-value profiles in Decision+ v8

The BIP-value GAM predicts runs scored on the play plus frozen RE288 after the play. For each hitter and pitcher it now uses mean exit velocity and two additive rates, barrel and ground ball, from the immediately preceding completed season. Opponent handedness and pitch family define each split. Fifty observations shrink player totals toward the league, and thirty shrink splits toward the player. Barrel rate uses classifiable batted balls and ground-ball rate uses typed batted balls. Current-pitch contact quality never enters the prediction row.

The 2022–23 fits shared 244,790 verified BIP. Arm A reproduced the v7 frozen-2021 EV model. Arm B updated only EV. Arm C, now active, added both rates to B. The other player contact-rate histories in the BIP-value GAM retain their 2021 definitions.

| Exploratory season | BIP RMSE, A / B / C | Supported factual swing MAE, A / B / C | V8 qualified hitters |
|---|---|---|---:|
| 2024 | .538366 / .538156 / .538114 | .177624 / .178276 / .178248 | 359 |
| 2025 | .540128 / .539993 / .539842 | .179332 / .179875 / .179742 | 337 |

Relative to B, C reduced MAE and positive bias for both outcomes in both seasons. Its paired RMSE intervals excluded zero in 2025 and included zero in 2024. A retained the lowest MAE and smallest mean bias. The local promotion replay matched the verified Arm C hitter scores within 1e-12 points across both years.

[Protocol](../model/experiments/bip_value_profiles_v1/PROTOCOL.md) · [Rebuild instructions](REPRODUCING.md)
