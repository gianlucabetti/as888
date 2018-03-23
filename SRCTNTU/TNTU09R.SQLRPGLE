000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011127     H*--------------------------------------------------------------*
000300051121     H*             - Anagrafico attività  -                         *
000400000000     H*--------------------------------------------------------------*
000500070801     FTntu09D   CF   E             WORKSTN
000600011127      *
000700011228     D KPJBA         E DS
000800070801     d tntu08ds      e ds
000900070801     d TIBS34ds      e ds
001000070801     d AZUTEds       e ds                  extname(AZUTE00F)
001100070801     d DDatiUte      e ds
001200070801     d att             s              3
001300070801     d savmese         s                   like(v01mese)
001400070801     d savanno         s                   like(v01anno)
001500070801     C*
001600110120     c                   if        *month > 1
001700070802     c                   eval      v01mese =*month - 1
001800070801     c                   eval      v01anno =*year
001900110120     c                   else
002000110120     c                   eval      v01mese = 12
002100110120     c                   eval      v01anno =*year -1
002200110120     c                   end
002300070801     C                   DO        *HIVAL
002400030117
002500051121     C                   EXFMT     v01
002600011127     C* FINE LAVORO
002700011228     C                   IF        *INKC
002800011228     C                   LEAVE
002900011228     c                   end
003000011228     C* controlli
003100051121     C                   EXSR      CTR01
003200011127     C   99              ITER
003300030117
003400070801     C* lancio
003500011228     C                   IF        *INKF
003600070801     C                   EXSR      SRtu08
003700011228     C                   LEAVE
003800011228     c                   end
003900011228     c                   enddo
004000011127     c*
004100000000     C                   SETON                                        LR
004200011127     C******************************************************
004300011127     C* CONTROLLI PRIMO VIDEO
004400011127     C******************************************************
004500011127     C     CTR01         BEGSR
004600070802     c                   setoff                                       990201
004700070801     c                   clear                   msgv
004800070801     c                   if        v01mese <> savmese or v01anno <> savanno
004900070801     C/EXEC SQL
005000070801     C+ DECLARE A1 CURSOR FOR SELECT TUS1ATT FROM tntus10f WHERE
005100070801     C+ TUS1AAS=:v01anno and TUS1MES=:v01mese
005200070801     C/END-EXEC
005300070801
005400070801     C/EXEC SQL
005500070801     C+ OPEN A1
005600070801     C/END-EXEC
005700070801
005800070801     C                   dou       *inlr
005900070801
006000070801     C/EXEC SQL
006100070801     C+ FETCH NEXT FROM A1 INTO :att
006200070801     C/END-EXEC
006300070801
006400070801     C                   select
006500070801     C                   when      sqlcod = 100
006600070801     c                   leave
006700070801     C                   when      sqlcod = 0
006800070801     c* dettaglio
006900070801     c                   seton                                        99
007000070801     c                   eval      msgv = 'Esiste una precedente elaborazione'
007100070801     c                   leave
007200070801     C                   endsl
007300070801     C*
007400070801     C                   enddo
007500070801
007600070801     C/EXEC SQL
007700070801     C+ CLOSE A1
007800070801     C/END-EXEC
007900070801     c                   end
008000070801     c                   eval      savmese = v01mese
008100070801     c                   eval      savanno = v01anno
008200070802     C* il mese deve essere al massimo quello in corso - 1 e devono essere
008300070802     c* passati almeno 15 gg per permettere l'elaborazione dei dati in
008400070802     c* filiale che avviene nel week
008500070802     c                   if        v01anno > *year
008600070802     c                   seton                                        0299
008700070802     c                   eval      msgv = 'Anno errato: non può essere superior-
008800070802     c                             e a quello in corso'
008900070802     c                   end
009000070802     c                   if        v01anno = *year and v01mese > *month -1
009100070802     c                   seton                                        0199
009200070802     c                   eval      msgv = 'Mese errato: non può essere superior-
009300070802     c                             e a quello in corso'
009400070802     c                   end
009500090914     c                   if        v01mese = *month -1 and *day < 14
009600140205     c                             and v01anno = *year
009700070802     c                   seton                                        019902
009800070802     c                   eval      msgv = 'ATTENZIONE:occorre attendere metà me-
009900070802     c                             se x elaborare.Dati non disponibili'
010000070802     c                   end
010100011127     C                   ENDSR
010200011228     c**********************************************************************
010300070801     c* lancio elaborazione
010400011228     c**********************************************************************
010500070801     C     srtu08        BEGSR
010600070801     C                   MOVEL     'TU08'        KCOAZ
010700070801     c                   eval      t08mm = v01mese
010800070801     c                   eval      t08aa  = v01anno
010900070801     C                   MOVEL(p)  tntu08ds      kpjbu
011000070801     C     KNMUS         IFEQ      *ALL'1'
011100070801     C                   CALL      'TNTU08R'
011200070801     C                   PARM                    KPJBA
011300070801     C                   ELSE
011400070801     C                   CALL      'BCH10'
011500070801     C                   PARM                    KPJBA
011600070801     C                   END
011700011228     C                   ENDSR
011800011127     C******************************************************
011900011127     C     *INZSR        BEGSR
012000011127     C******************************************************
012100011127     C*
012200011127     C     *ENTRY        PLIST
012300051121     C                   PARM                    KPJBA
012400070801      * Reperisco dati job
012500070801     c                   exsr      DatiJob
012600030123
012700070801     C                   ENDSR
012800070801      *---------------------------------------------------------------*
012900070801      * Reperimento Dati del job (Utente/Operativi)                   *
013000070801      *---------------------------------------------------------------*
013100070801     c     DatiJob       BEGSR
013200070801      *
013300070801     c     *dtaara       define    §azute        azuteds
013400070801     c     *dtaara       define    §datiute      ddatiute
013500070801      *
013600070801     c                   in(E)     *dtaara
013700070801     c                   IF        %ERROR or RSUT = *blanks
013800070801     c                   clear                   Tibs34Ds
013900070801     c                   call      'TIBS34R'
014000070801     c                   parm                    Tibs34Ds
014100070801     c                   in        *dtaara
014200070801     c                   ENDIF
014300070801      *
014400070801     c                   ENDSR
