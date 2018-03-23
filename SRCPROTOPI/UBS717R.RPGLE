000100170803     D*
000200170803     D iUBS717VAB      S                   like(EDIVABDS)
000300170803     D iUBS717ENV      s              3A
000400170803     D*                                         Ambiente: 'VWS' => VAS WEB SERVICE ASCII
000500170803     D*                                                   'VWB' => VAS WEB SERVICE BASE64
000600170803     D iUBS717OPZ      s              4A
000700170803     D iUBS717KSU      s              7A
000800170804     D iUBS717VPC      s            256A
000900170522     D*                                         Dati
001000170803     D oUBS717Esito    s              1S 0
001100170816     D*   -1 = Generic Error
001200170816     D*   -2 = Parameter Error
001300170816     D*   -3 = Routing Calculation Error
001400170816     D*   -4 = Parcel Number Error
001500170816     D*   -5 = Parcel Label Error
001600170825     D oUBS717MSG      s            256A
001700170803     D oUBS717SI95     s                   like(UBSI95DS)
001800170804     D oUBS717DLEN     s             10I 0
001900170803     D oUBS717DATI     s               A   LEN(1000000)
