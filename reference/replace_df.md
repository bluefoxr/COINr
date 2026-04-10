# Replace multiple values in a data frame

Given a data frame (or vector), this function replaces values according
to a look up table or dictionary. In COINr this may be useful for
exchanging categorical data with numeric scores, prior to assembly. Or
for changing codes.

## Usage

``` r
replace_df(df, lookup)
```

## Arguments

- df:

  A data frame or a vector

- lookup:

  A data frame with columns `old` (the values to be replaced) and `new`
  the values to replace with. See details.

## Value

A data frame with replaced values

## Details

The lookup data frame must not have any duplicated values in the `old`
column. This function looks for exact matches of elements of the `old`
column and replaces them with the corresponding value in the `new`
column. For each row of `lookup`, the class of the old value must match
the class of the new value. This is to keep classes of data frames
columns consistent. If you wish to replace with a different class, you
should convert classes in your data frame before using this function.

This function replaces the now-defunct `replaceDF()` from COINr \< v1.0.

## Examples

``` r
# replace sub-pillar codes in ASEM indicator metadata
codeswap <- data.frame(old = c("Conn", "Sust"), new = c("SI1", "SI2"))
# swap codes in both iMeta
replace_df(ASEM_iMeta, codeswap)
#>    Level         iCode
#> 1      1           LPI
#> 2      1       Flights
#> 3      1          Ship
#> 4      1          Bord
#> 5      1          Elec
#> 6      1           Gas
#> 7      1      ConSpeed
#> 8      1         Cov4G
#> 9      1         Goods
#> 10     1      Services
#> 11     1           FDI
#> 12     1        PRemit
#> 13     1       ForPort
#> 14     1          Embs
#> 15     1          IGOs
#> 16     1        UNVote
#> 17     1     CostImpEx
#> 18     1        Tariff
#> 19     1          TBTs
#> 20     1        TIRcon
#> 21     1          RTAs
#> 22     1          Visa
#> 23     1         StMob
#> 24     1      Research
#> 25     1           Pat
#> 26     1      CultServ
#> 27     1      CultGood
#> 28     1       Tourist
#> 29     1      MigStock
#> 30     1          Lang
#> 31     1         Renew
#> 32     1      PrimEner
#> 33     1           CO2
#> 34     1        MatCon
#> 35     1        Forest
#> 36     1       Poverty
#> 37     1         Palma
#> 38     1      TertGrad
#> 39     1     FreePress
#> 40     1        TolMin
#> 41     1          NGOs
#> 42     1           CPI
#> 43     1        FemLab
#> 44     1       WomParl
#> 45     1       PubDebt
#> 46     1      PrivDebt
#> 47     1       GDPGrow
#> 48     1         RDExp
#> 49     1          NEET
#> 50     2      Physical
#> 51     2      ConEcFin
#> 52     2     Political
#> 53     2        Instit
#> 54     2           P2P
#> 55     2       Environ
#> 56     2        Social
#> 57     2      SusEcFin
#> 58     3           SI1
#> 59     3           SI2
#> 60     4         Index
#> 61    NA     GDP_group
#> 62    NA   GDPpc_group
#> 63    NA     Pop_group
#> 64    NA EurAsia_group
#> 65    NA          Area
#> 66    NA        Energy
#> 67    NA           GDP
#> 68    NA    Population
#>                                                             iName Direction
#> 1                                     Logistics Performance Index         1
#> 2                        International flights passenger capacity         1
#> 3                               Liner Shipping Connectivity Index         1
#> 4                                                Border crossings         1
#> 5                                            Trade in electricity         1
#> 6                                                    Trade in gas         1
#> 7                                        Average connection speed         1
#> 8              Population covered by at least a 4G mobile network         1
#> 9                                                  Trade in goods         1
#> 10                                              Trade in services         1
#> 11                                      Foreign direct investment         1
#> 12                       Personal remittances (received and paid)         1
#> 13            Foreign portfolio investment liabilities and assets         1
#> 14                                              Embassies network         1
#> 15 Participation in international intergovernmental organisations         1
#> 16                                            UN voting alignment         1
#> 17                                          Cost to export/import        -1
#> 18                                               Mean tariff rate        -1
#> 19                                    Technical barriers to trade        -1
#> 20                                    Signatory of TIR Convention         1
#> 21                                      Regional trade agreements         1
#> 22                                   Visa-free or visa-on-arrival         1
#> 23           International student mobility in tertiary education         1
#> 24             Research outputs with international collaborations         1
#> 25                               Patents with foreign co-inventor         1
#> 26                                     Trade in cultural services         1
#> 27                                        Trade in cultural goods         1
#> 28                           Tourist arrivals at national borders         1
#> 29                                                  Migrant stock         1
#> 30                                         Common languages users         1
#> 31             Renewable energy in total final energy consumption         1
#> 32                                     Primary energy use per GDP        -1
#> 33                                       CO2 emissions per capita        -1
#> 34                       Domestic material consumption per capita        -1
#> 35                                                Net forest loss        -1
#> 36          Population living below the intenational poverty line        -1
#> 37                                                    Palma Index        -1
#> 38                                             Tertiary graduates         1
#> 39                                           Freedom of the press        -1
#> 40                                       Tolerance for minorities        -1
#> 41       Presence of international non-governmental organisations         1
#> 42                                   Corruption Perceptions Index         1
#> 43                              Female labour-force participation         1
#> 44                  Women's participation in national parliaments         1
#> 45                             Public debt as a percentage of GDP        -1
#> 46   Private debt, loans and debt securities as percentage of GDP        -1
#> 47                                          GDP per capita growth         1
#> 48                         R&D expenditure as a percentage of GDP         1
#> 49   Proportion of youth not in education, employment or training        -1
#> 50                                                       Physical         1
#> 51                                   Economic and Financial (Con)         1
#> 52                                                      Political         1
#> 53                                                  Institutional         1
#> 54                                               People to People         1
#> 55                                                  Environmental         1
#> 56                                                         Social         1
#> 57                                   Economic and Financial (Sus)         1
#> 58                                                   Connectivity         1
#> 59                                                 Sustainability         1
#> 60                                       Sustainable Connectivity         1
#> 61                                                      GDP group        NA
#> 62                                           GDP per capita group        NA
#> 63                                               Population group        NA
#> 64                                                 Europe or Asia        NA
#> 65                                                      Land area        NA
#> 66                                             Energy consumption        NA
#> 67                                                            GDP        NA
#> 68                                                     Population        NA
#>    Weight                                                   Unit       Target
#> 1       1                                              Score 1-5 4.118031e+00
#> 2       1                                         Thousand seats 2.003327e+02
#> 3       1                                                  Score 2.011338e+01
#> 4       1                                    Number of crossings 1.159000e+02
#> 5       1                                                    TWh 1.046706e+02
#> 6       1                                         Billion tonnes 9.006042e+01
#> 7       1                                                   Mbps 2.744500e+01
#> 8       1                                                Percent 9.500000e+01
#> 9       1                                           Trillion USD 1.823596e+03
#> 10      1                                            Million USD 6.243225e+02
#> 11      1                                            Billion USD 7.182650e+01
#> 12      1                                            Million USD 2.870772e+01
#> 13      1                                            Million USD 1.007166e+04
#> 14      1                                                 Number 9.640000e+01
#> 15      1                                                 Number 3.166500e+02
#> 16      1                                                  Score 4.281453e+01
#> 17      1                                            Current USD 4.960000e+01
#> 18      1                                                Percent 5.265000e-01
#> 19      1               Number of measures (initiated, in force) 8.865000e+01
#> 20      1                                       (1 (yes)/0 (no)) 9.500000e-01
#> 21      1                        Number of bilateral connections 4.375000e+01
#> 22      1                                    Number of countries 8.745000e+01
#> 23      1                       Thousands international students 4.228385e+02
#> 24      1                                                 Number 9.152890e+04
#> 25      1                                                 Number 2.633130e+03
#> 26      1                                            Million USD 9.091595e+00
#> 27      1                                            Million USD 7.074690e+01
#> 28      1                                       Number of people 7.844775e+01
#> 29      1                                       Thousands people 1.038616e+01
#> 30      1                                                  Score 2.041194e+01
#> 31      1                                                Percent 6.167833e+01
#> 32      1 kg of oil equivalent per $1000 GDP (constant 2011 PPP) 6.042716e+01
#> 33      1                                      Tonnes per capita 1.388576e+00
#> 34      1                                      Tonnes per capita 4.367344e+00
#> 35      1                                                Percent 1.902348e+00
#> 36      1                                                Percent 1.135000e+00
#> 37      1                                                  Ratio 9.670000e-01
#> 38      1                                                Percent 3.582519e+01
#> 39      1                                                  Score 1.195000e+01
#> 40      1                                                  Score 1.535000e+00
#> 41      1           Number of secretariats per 1,000 inhabitants 1.732800e+03
#> 42      1                                                  Score 8.560000e+01
#> 43      1                                                  Ratio 9.909484e-01
#> 44      1                                                Percent 4.161826e+01
#> 45      1                                                Percent 1.506850e+01
#> 46      1                                                Percent 3.975949e+01
#> 47      1                                                Percent 7.166495e+00
#> 48      1                                                Percent 4.020985e+00
#> 49      1                                                Percent 2.675000e+00
#> 50      1                                                  Score           NA
#> 51      1                                                  Score           NA
#> 52      1                                                  Score           NA
#> 53      1                                                  Score           NA
#> 54      1                                                  Score           NA
#> 55      1                                                  Score           NA
#> 56      1                                                  Score           NA
#> 57      1                                                  Score           NA
#> 58      1                                                  Score           NA
#> 59      1                                                  Score           NA
#> 60      1                                                  Score           NA
#> 61     NA                                                   <NA>           NA
#> 62     NA                                                   <NA>           NA
#> 63     NA                                                   <NA>           NA
#> 64     NA                                                   <NA>           NA
#> 65     NA                                     Thousand square km           NA
#> 66     NA                                                   Unit           NA
#> 67     NA                                                 USD Bn           NA
#> 68     NA                                              Thousands           NA
#>    Denominator    Parent        Type
#> 1         <NA>  Physical   Indicator
#> 2   Population  Physical   Indicator
#> 3         <NA>  Physical   Indicator
#> 4         Area  Physical   Indicator
#> 5       Energy  Physical   Indicator
#> 6       Energy  Physical   Indicator
#> 7         <NA>  Physical   Indicator
#> 8         <NA>  Physical   Indicator
#> 9          GDP  ConEcFin   Indicator
#> 10         GDP  ConEcFin   Indicator
#> 11         GDP  ConEcFin   Indicator
#> 12         GDP  ConEcFin   Indicator
#> 13         GDP  ConEcFin   Indicator
#> 14        <NA> Political   Indicator
#> 15        <NA> Political   Indicator
#> 16        <NA> Political   Indicator
#> 17        <NA>    Instit   Indicator
#> 18        <NA>    Instit   Indicator
#> 19        <NA>    Instit   Indicator
#> 20        <NA>    Instit   Indicator
#> 21        <NA>    Instit   Indicator
#> 22        <NA>    Instit   Indicator
#> 23  Population       P2P   Indicator
#> 24         GDP       P2P   Indicator
#> 25         GDP       P2P   Indicator
#> 26         GDP       P2P   Indicator
#> 27         GDP       P2P   Indicator
#> 28  Population       P2P   Indicator
#> 29  Population       P2P   Indicator
#> 30        <NA>       P2P   Indicator
#> 31        <NA>   Environ   Indicator
#> 32        <NA>   Environ   Indicator
#> 33        <NA>   Environ   Indicator
#> 34        <NA>   Environ   Indicator
#> 35        <NA>   Environ   Indicator
#> 36        <NA>    Social   Indicator
#> 37        <NA>    Social   Indicator
#> 38        <NA>    Social   Indicator
#> 39        <NA>    Social   Indicator
#> 40        <NA>    Social   Indicator
#> 41  Population    Social   Indicator
#> 42        <NA>    Social   Indicator
#> 43        <NA>    Social   Indicator
#> 44        <NA>    Social   Indicator
#> 45        <NA>  SusEcFin   Indicator
#> 46        <NA>  SusEcFin   Indicator
#> 47        <NA>  SusEcFin   Indicator
#> 48        <NA>  SusEcFin   Indicator
#> 49        <NA>  SusEcFin   Indicator
#> 50        <NA>       SI1   Aggregate
#> 51        <NA>       SI1   Aggregate
#> 52        <NA>       SI1   Aggregate
#> 53        <NA>       SI1   Aggregate
#> 54        <NA>       SI1   Aggregate
#> 55        <NA>       SI2   Aggregate
#> 56        <NA>       SI2   Aggregate
#> 57        <NA>       SI2   Aggregate
#> 58        <NA>     Index   Aggregate
#> 59        <NA>     Index   Aggregate
#> 60        <NA>      <NA>   Aggregate
#> 61        <NA>      <NA>       Group
#> 62        <NA>      <NA>       Group
#> 63        <NA>      <NA>       Group
#> 64        <NA>      <NA>       Group
#> 65        <NA>      <NA> Denominator
#> 66        <NA>      <NA> Denominator
#> 67        <NA>      <NA> Denominator
#> 68        <NA>      <NA> Denominator
```
