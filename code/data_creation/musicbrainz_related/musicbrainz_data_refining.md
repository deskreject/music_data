# Method of reducing the amount of releases and songs

# First of all, all releases under the "[no label]" category of labels were removed
- 25.000 releases were removed

# Second, the "broadcast" category was removed
- 1030 releases removed

# Third Removal of "other" category

Second, the "other" category was analyzed to identify what falls under this category with gemini and chatgpt (2.0 experimental and O1). Here, the analysis revealed that it was mainly audio books and spoken word elements, with a few instances of compilation/greatest hits (according to gemini, 109 elements). 
Used the following to identify potentially relevant releases under the "other" category related to compliations with the following regular expressions appearing in the name of the release according to suggestion by gemini 2.0 experimental:
    patterns = [
        r".*(greatest hits|best of|collection|anthology|years|sounds of|classics).*",  # Common keywords
        r".*(compilation|sampler).*", #compilation and sampler
        r".*(\bvol\b|\bvolume\b).*\d+.*", # Volumes
        r".*\d{4}[s]?\b.*", # Decades
        r".*(the essential|ultimate|definitive).*",
        r".*(gold|platinum|diamond|anniversary).*",
        r".*edition.*",
        r".*(\bHits\b).*"
    ]
- recategorized 900 elements
- removed 10.500 elements

Total of 36.500 releases of 1.000.000 removed (marginal)

# removal of songs releated to the removed releases

Removal of any duplicate id_recording entries
- reduces dataset to about 8.4 million

Removal of rows related to the removed release ids
- reduces dataset to about 8 million

 