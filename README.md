# Water depth and wading-bird nesting in the Everglades

BIOL6130 Research Project — u8179386

## Extended abstract

### Objective

Water management in the Florida Everglades presents a central conservation dilemma: protected species require different hydrological conditions. Snail kites depend on sustained flooding, while Cape Sable seaside sparrows require dry grasslands. Using monitoring data from 1992–2024, this study focuses on wood storks and white ibises, two indicators of Everglades restoration, to examine how seasonal water depth influences nesting and inform species-specific conservation.

### Main Findings and Predictions

Each 10 cm increase in breeding-season depth was associated with 28% fewer wood stork pairs and 31% fewer white ibis pairs. For preceding wet-season depth, the corresponding increases were 46% and 28%. These directions remained consistent across robustness checks.

Density-dependent growth models partly supported the prediction for wood storks, with only the positive wet-season association supported. Neither seasonal association was supported for white ibises.

Across six species, birds requiring concentrated prey showed stronger negative breeding-season associations than flexible foragers (group contrast 0.74, 95% confidence interval 0.64–0.85; p = 0.003). Both groups showed positive preceding wet-season associations.

An exploratory joint comparison for 1996–2021, added after the main results, found a more positive breeding-season association for snail kites than for storks and ibises (p = 0.004 and 0.003). Wet-season associations were positive in all three species.

### Research Significance

These findings highlight a fundamental dilemma in conservation biology: hydrological conditions that benefit one protected species can create challenges for another. Effective Everglades restoration may therefore require more precise, species-specific approaches that consider seasonal water dynamics, foraging strategies, and breeding requirements. By examining how ecological differences shape responses to water levels, this study aims to inform conservation strategies that accommodate the competing needs of multiple protected species.

## Hypotheses and Predictions

1. No single water level maximises nesting: Deep breeding-season water reduces nesting, while a wet preceding season followed by drawdown supports it.

   Prediction: Greater breeding-season depth (March–June) predicts fewer wood stork and white ibis nesting pairs. Greater preceding wet-season depth (June–November) predicts more pairs the following year.

2. Wood storks and white ibises are focal species for breeding-season water management (Lambeck, 1997): Species requiring concentrated prey are more sensitive to deep breeding-season water, while wet-season flooding benefits both foraging groups.

   Prediction: Across six wading-bird species, breeding-season depth more strongly suppresses nesting in prey-concentration-dependent species (wood stork, white ibis, snowy egret) than in flexible foragers (great egret, tricolored heron, great blue heron). Greater preceding wet-season depth predicts increased nesting in both groups.

## Data

Annual maximum nesting-pair counts and foraging classifications come from the University of Florida's Everglades Wading Bird dataset (weecology/EvergladesWadingBird, Zenodo). Daily water levels from 33 stations and ground elevations come from USGS EDEN. Annual snail kite nest counts (1996–2021), used only in the exploratory comparison, come from the University of Florida's snail kite monitoring data (Fletcher 2024, figshare).

[`data/PROVENANCE.md`](data/PROVENANCE.md) lists, for every data file, where it came from, who collected it, its licence and how to cite it. The repository's `LICENSE` (MIT) covers the code; the data files keep their original licences.

## Reproducing the analysis

**Requirements:**

- R ≥ 4.5 (developed with R 4.5.2);
- Quarto ≥ 1.4 (developed with Quarto 1.9.37);
- internet access the first time, so that `renv` can install the packages.

The packages come from a dated Posit Package Manager snapshot (2026-11-03), which provides binary builds for Windows and macOS, so no compiler is needed.

**Steps:**

1. Clone or download this repository and open a terminal in its folder.
2. Start R in that folder (renv activates through `.Rprofile`) and run:

   ```r
   renv::restore()
   ```

3. Render the analysis from the terminal:

   ```bash
   quarto render analysis.qmd
   ```

   If Quarto cannot find R, set the environment variable `QUARTO_R` to the folder that contains `Rscript`. On Windows, for example, that is `C:\Program Files\R\R-4.5.2\bin`.

**Outputs:**

- `analysis.html`: the complete analysis, with code;
- `results/*.csv`: every reported number;
- `figures/*.png` and `figures/*.pdf`: figures F1–F6 and supplementary figures S1–S3.

Once the packages are installed, rendering takes about 30 seconds. Randomness (the DHARMa residual simulations and the coefficient draws for K) uses seed `20261104`.

**Checks:** `Rscript -e "testthat::test_dir('tests/testthat')"` runs the data, depth-index, model and figure tests.

## Generative-AI declaration

Generative AI (Claude Code by Anthropic, model Claude Opus 5.5) was used as a coding and analysis assistant. It wrote the figure code, the GLM model code, the reproducibility set-up and the data provenance statement.

## References

Lambeck RJ (1997) Focal species: a multi-species umbrella for nature conservation. *Conservation Biology* 11: 849–856.
