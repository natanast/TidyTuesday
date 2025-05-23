# Southern Resident Killer Whale Encounters

The data this week comes from the Center for Whale Research (CWR), the leading organization monitoring and studying Southern Resident killer whales in their critical habitat: the Pacific Northwest’s Salish Sea. Each encounter is hosted on its own webpage at [whaleresearch.com](https://www.whaleresearch.com/encounters). Jadey Ryan scraped the encounter data from CWR's website as a personal project to learn web scraping and [presented the process](https://jadeynryan.github.io/orcas/) at a Seattle R-Ladies meetup in 2023. The scraping functions and cleaning code for 2017 - 2024 encounters can be found in the [{orcas} R package](https://github.com/jadeynryan/orcas).

The dataset is mostly tidy but not completely clean. There are still missing values and typos, as evident from some encounters having a negative duration.

> An Encounter refers to any time we observe killer whales (orcas), from one of CWR's research boats or land, where at least one individual is identified and photographed. Typically, 2-4 staff are involved in an encounter. Once we come into contact with whales (ie. within distance of identifying individuals by sight) we have begun our encounter. During an encounter, our main goal is to photograph every individual present from both the left and right side.

Which pods or ecotypes have the longest duration encounters with CWR researchers? Are there trends in where orca encounters occur over time?

Thank you to [Jadey Ryan](https://github.com/jadeynryan) for curating this week's dataset.
