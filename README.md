# JM_biol

Technical working paper scaffold for evaluating biological data used in the
SPRFMO joint jack mackerel model assessment.

The report is in `JM_biol.qmd`. It is designed to read model `1.14` inputs from
the neighboring `jjm` repository:

- `../jjm/assessment/input/1.14.dat`
- `../jjm/assessment/config/h1_1.14.ctl`
- `../jjm/assessment/config/h2_1.14.ctl`

The `data/model_1.14` directory is intended only as a convenient access point.
Keep authoritative assessment inputs in `../jjm/assessment`.

Render with:

```sh
quarto render JM_biol.qmd
```

