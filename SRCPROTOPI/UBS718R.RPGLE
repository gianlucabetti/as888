000100170803     D*
000200170906     D iUBS718LANG     s              2A
000300170908     D iUBS718FGS      s              3S 0
000400170906     D iUBS718CCM      s              7S 0
000500170906     D iUBS718REF      s             15A
000600170906     D iUBS718REFTYP   s              1A
000700170906     D iUBS718LBLTYP   s              1A
000800170906     D*                                       Label Type: 'Z' = ZPL
000900170906     D*                                                   'B' = ZPL BASE64
001000170906     D*                                                   'P' = PDF
001100170906     D*                                                   'T' = TIF
001200170906     D iUBS718VPC      s            256A
001300170522     D*                                         Dati
001400170906     D oUBS718Esito    s              1S 0
001500170906     D*    0 = OK
001600170816     D*   -1 = Generic Error
001700170816     D*   -2 = Parameter Error
001800170906     D*   -3 = Shipment not Found
001900170906     D*   -4 = Shipment not Unique
002000170906     D*   -5 = Label genaration error
002100170906     D oUBS718MSG      s            128A
002200170925     D oUBS718NCL      s              5S 0
002300170925     D oUBS718LNP      s              3S 0
002400170925     D oUBS718LNA      s              3S 0
002500170925     D oUBS718NRS      s              2S 0
002600170925     D oUBS718ZNC      s              2S 0
002700170925     D oUBS718NCD      s              7S 0
002800170925     D oUBS718NCA      s              7S 0
002900170906     D oUBS718DLEN     s             10I 0
003000170906     D oUBS718DATI     s               A   LEN(1000000)
