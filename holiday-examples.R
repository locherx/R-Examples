## Feiertage bestimmen
## vgl. auch https://www.feiertagskalender.ch/index.php?geo=3056

require(timeDate)

year <- 2019:2020

NewYearsDay(year)
CHBerchtoldsDay(year)
GoodFriday(year)

EasterMonday(year)

CHAscension(year)

Pentecost(year)

LaborDay(year)

CHConfederationDay(year)
ChristmasDay(year)
BoxingDay(year)

holidayZURICH(year)

hl <-
    holiday(year = year,
            Holiday = c("NewYearsDay", "CHBerchtoldsDay", "GoodFriday", "EasterMonday",
                "CHAscension", "PentecostMonday", "ChristmasEve", "BoxingDay"))
hl
sort(hl)
