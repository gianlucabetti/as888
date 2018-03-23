000100170620     H*--------------------------------------------------------------------------------------------*
000200170627     H* DPD GeoRDB 2017 - Calcolo Instradamento - TEST
000300170620     H*--------------------------------------------------------------------------------------------*
000400170620     H DECEDIT('0,') DATEDIT(*DMY.)
000500170620     F*------------------
000600170620     F* DATA BASE
000700170620     F*------------------
000800170620
000900170620     D*------------------
001000170620     D* DS ESTERNE
001100170620     D*------------------
001200170627     D TISID4DS      e ds
001300170620
001400170620
001500170620     C*--------------------------------------------------------------------------------------------*
001600170620     C* Main lines
001700170620     C*--------------------------------------------------------------------------------------------*
001800170620     C*
001900170620     C* Valorizzazioni
002000170629     C                   clear                   TISID4DS
002100170627     C                   eval      ISID4DRI   = 20170704
002200170627     C                   eval      ISID4LNP   = 001
002300170629     C                   eval      ISID4NZD   = 'GB'
002400170629     C                   eval      ISID4CAD   = 'LS18  4TR'
002500170629     C                   eval      ISID4LOD   = 'LONDRA'
002600170627     C                   eval      ISID4SUBC  = *blanks
002700170627     C                   eval      ISID4TRADS = *blanks
002800170627     C                   eval      ISID4SORC  = 101
002900170627     C                   call      'TISID4R'
003000170627     C                   parm                    TISID4DS
003100170620     C*
003200170627     C     OSID4ERRL     dsply
003300170629     C*
003400170629     C* Valorizzazioni
003500170629     C                   clear                   TISID4DS
003600170629     C                   eval      ISID4DRI   = 20170704
003700170629     C                   eval      ISID4LNP   = 007
003800170629     C                   eval      ISID4NZD   = 'S'
003900170629     C                   eval      ISID4CAD   = '13033'
004000170629     C                   eval      ISID4LOD   = 'SALACABIR'
004100170629     C                   eval      ISID4SUBC  = *blanks
004200170629     C                   eval      ISID4TRADS = *blanks
004300170629     C                   eval      ISID4SORC  = 136
004400170629     C                   call      'TISID4R'
004500170629     C                   parm                    TISID4DS
004600170629     C*
004700170629     C     OSID4ERRL     dsply
004800170629     C*
004900170629     C* Valorizzazioni
005000170629     C                   clear                   TISID4DS
005100170629     C                   eval      ISID4DRI   = 20170704
005200170629     C                   eval      ISID4LNP   = 007
005300170629     C                   eval      ISID4NZD   = 'IRL'
005400170629     C                   eval      ISID4CAD   = 'EIRE'
005500170629     C                   eval      ISID4LOD   = 'REJKAVICK'
005600170629     C                   eval      ISID4SUBC  = *blanks
005700170629     C                   eval      ISID4TRADS = *blanks
005800170629     C                   eval      ISID4SORC  = 136
005900170629     C                   call      'TISID4R'
006000170629     C                   parm                    TISID4DS
006100170629     C*
006200170629     C     OSID4ERRL     dsply
006300170620     C*
006400170620     C                   seton                                        lr
006500170620
