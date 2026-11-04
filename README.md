# Water depth and wading-bird nesting in the Everglades

BIOL6130 Research Project — u8179386

## Extended abstract

### Objective

Water management in the Florida Everglades presents a central conservation dilemma: protected species require different hydrological conditions. Snail kites depend on sustained flooding, while Cape Sable seaside sparrows require dry grasslands. Using monitoring data from 1992–2024, this study focuses on wood storks and white ibises, two indicators of Everglades restoration, to examine how seasonal water depth influences nesting and inform species-specific conservation.

### Main Findings and Predictions

Preliminary analyses suggest that deeper breeding-season water (March–June) is associated with fewer nesting pairs, while wetter preceding wet seasons (June–November) are associated with increased nesting the following year. These patterns suggest that seasonal flooding supports prey production, while subsequent drawdown concentrates prey for breeding birds.

Across six wading-bird species, responses also appear to vary with foraging strategy. Species requiring concentrated prey (wood stork, white ibis, snowy egret) are expected to experience stronger reductions in nesting under deep breeding-season water than flexible foragers (great egret, tricolored heron, great blue heron). Population-growth models will further examine these relationships while accounting for density dependence.

### Research Significance

These findings highlight a fundamental dilemma in conservation biology: hydrological conditions that benefit one protected species can create challenges for another. Effective Everglades restoration may therefore require more precise, species-specific approaches that consider seasonal water dynamics, foraging strategies, and breeding requirements. By examining how ecological differences shape responses to water levels, this study aims to inform conservation strategies that accommodate the competing needs of multiple protected species.

## Hypotheses and Predictions

1. No single water level maximises nesting: Deep breeding-season water reduces nesting, while a wet preceding season followed by drawdown supports it.

   Prediction: Greater breeding-season depth (March–June) predicts fewer wood stork and white ibis nesting pairs. Greater preceding wet-season depth (June–November) predicts more pairs the following year.

2. Wood storks and white ibises are focal species for breeding-season water management (Lambeck, 1997): Species requiring concentrated prey are more sensitive to deep breeding-season water, while wet-season flooding benefits both foraging groups.

   Prediction: Across six wading-bird species, breeding-season depth more strongly suppresses nesting in prey-concentration-dependent species (wood stork, white ibis, snowy egret) than in flexible foragers (great egret, tricolored heron, great blue heron). Greater preceding wet-season depth predicts increased nesting in both groups.

## Data

Annual maximum nesting-pair counts and foraging classifications come from the University of Florida's Everglades Wading Bird dataset (weecology/EvergladesWadingBird, Zenodo). Daily water levels from 33 stations and ground elevations come from USGS EDEN.

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
- `figures/*.png` and `figures/*.pdf`: poster figures F1–F5 and supplementary figures S1–S2.

Once the packages are installed, rendering takes about 30 seconds. Randomness (the DHARMa residual simulations and the coefficient draws for K) uses seed `20261104`.

**Checks:** `Rscript -e "testthat::test_dir('tests/testthat')"` runs the data, depth-index, model and figure tests.

## Generative-AI declaration

Generative AI (Claude Code by Anthropic, model Claude Opus 5.5) was used as a coding and analysis assistant. It wrote the figure code, the GLM model code, the reproducibility set-up and the data provenance statement.

## References

Lambeck RJ (1997) Focal species: a multi-species umbrella for nature conservation. *Conservation Biology* 11: 849–856.
