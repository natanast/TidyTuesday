# ISO Country Codes

We've referenced countries and country codes in many past datasets, but we've never looked closely at the ISO 3166 standard that defines these codes.

Wikipedia says:

> ISO 3166 is a standard published by the International Organization for 
> Standardization (ISO) that defines codes for the names of countries, dependent 
> territories, special areas of geographical interest, and their principal 
> subdivisions (e.g., provinces or states). The official name of the standard 
> is Codes for the representation of names of countries and their subdivisions.

The dataset this week comes from the {[ISOcodes](https://cran.r-project.org/package=ISOcodes)} R package. It consists of three tables:

- `countries`: Country codes from ISO 3166-1.
- `country_subdivisions`: Country subdivision code from ISO 3166-2.
- `former_countries`: Code for formerly used names of countries from ISO 3166-3.

Thank you to [Jon Harmon](https://github.com/jonthegeek) for curating this week's dataset.
