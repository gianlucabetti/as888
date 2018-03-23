000200000000     H DECEDIT('0,') DATEDIT(*DMY.)
000300011127     H*--------------------------------------------------------------*
000400051121     H*             - Anagrafico attività  -                         *
000500000000     H*--------------------------------------------------------------*
001101070801     FTntu09D   CF   E             WORKSTN
001600011127      *
001900011228     D KPJBA         E DS
001901070801     d tntu08ds      e ds
001903070801     d TIBS34ds      e ds
001904070801     d AZUTEds       e ds                  extname(AZUTE00F)
001905070801     d DDatiUte      e ds
001906070801     d att             s              3
007200070801     d savmese         s                   like(v01mese)
007201070801     d savanno         s                   like(v01anno)
007700070801     C*
007701110120     c                   if        *month > 1
007702070802     c                   eval      v01mese =*month - 1
007703070801     c                   eval      v01anno =*year
007704110120     c                   else
007705110120     c                   eval      v01mese = 12
007706110120     c                   eval      v01anno =*year -1
007707110120     c                   end
007800070801     C                   DO        *HIVAL
008300030117
009600051121     C                   EXFMT     v01
009900011127     C* FINE LAVORO
010000011228     C                   IF        *INKC
010200011228     C                   LEAVE
010300011228     c                   end
011800011228     C* controlli
011900051121     C                   EXSR      CTR01
013400011127     C   99              ITER
013500030117
013600070801     C* lancio
013700011228     C                   IF        *INKF
013800070801     C                   EXSR      SRtu08
014400011228     C                   LEAVE
014500011228     c                   end
014600011228     c                   enddo
014700011127     c*
014800000000     C                   SETON                                        LR
021000011127     C******************************************************
021100011127     C* CONTROLLI PRIMO VIDEO
021200011127     C******************************************************
021300011127     C     CTR01         BEGSR
021400070802     c                   setoff                                       990201
021401070801     c                   clear                   msgv
021402070801     c                   if        v01mese <> savmese or v01anno <> savanno
021403070801     C/EXEC SQL
021404070801     C+ DECLARE A1 CURSOR FOR SELECT TUS1ATT FROM tntus10f WHERE
021405070801     C+ TUS1AAS=:v01anno and TUS1MES=:v01mese
021406070801     C/END-EXEC
021407070801
021408070801     C/EXEC SQL
021409070801     C+ OPEN A1
021410070801     C/END-EXEC
021411070801
021412070801     C                   dou       *inlr
021413070801
021414070801     C/EXEC SQL
021415070801     C+ FETCH NEXT FROM A1 INTO :att
021416070801     C/END-EXEC
021417070801
021418070801     C                   select
021419070801     C                   when      sqlcod = 100
021420070801     c                   leave
021421070801     C                   when      sqlcod = 0
021422070801     c* dettaglio
021423070801     c                   seton                                        99
021424070801     c                   eval      msgv = 'Esiste una precedente elaborazione'
021425070801     c                   leave
021426070801     C                   endsl
021427070801     C*
021428070801     C                   enddo
021429070801
021430070801     C/EXEC SQL
021431070801     C+ CLOSE A1
021432070801     C/END-EXEC
021433070801     c                   end
021434070801     c                   eval      savmese = v01mese
021435070801     c                   eval      savanno = v01anno
021436070802     C* il mese deve essere al massimo quello in corso - 1 e devono essere
039500070802     c* passati almeno 15 gg per permettere l'elaborazione dei dati in
039501070802     c* filiale che avviene nel week
039502070802     c                   if        v01anno > *year
039503070802     c                   seton                                        0299
039504070802     c                   eval      msgv = 'Anno errato: non può essere superior-
039505070802     c                             e a quello in corso'
039506070802     c                   end
039507070802     c                   if        v01anno = *year and v01mese > *month -1
039508070802     c                   seton                                        0199
039509070802     c                   eval      msgv = 'Mese errato: non può essere superior-
039510070802     c                             e a quello in corso'
039511070802     c                   end
039512090914     c                   if        v01mese = *month -1 and *day < 14
039513070802     c                   seton                                        019902
039514070802     c                   eval      msgv = 'ATTENZIONE:occorre attendere metà me-
039515070802     c                             se x elaborare.Dati non disponibili'
039516070802     c                   end
039600011127     C                   ENDSR
048500011228     c**********************************************************************
048600070801     c* lancio elaborazione
048700011228     c**********************************************************************
048800070801     C     srtu08        BEGSR
048801070801     C                   MOVEL     'TU08'        KCOAZ
048802070801     c                   eval      t08mm = v01mese
048803070801     c                   eval      t08aa  = v01anno
048804070801     C                   MOVEL(p)  tntu08ds      kpjbu
048807070801     C     KNMUS         IFEQ      *ALL'1'
048808070801     C                   CALL      'TNTU08R'
048809070801     C                   PARM                    KPJBA
048810070801     C                   ELSE
048811070801     C                   CALL      'BCH10'
048812070801     C                   PARM                    KPJBA
048813070801     C                   END
053400011228     C                   ENDSR
055700011127     C******************************************************
055800011127     C     *INZSR        BEGSR
055900011127     C******************************************************
056000011127     C*
056100011127     C     *ENTRY        PLIST
056200051121     C                   PARM                    KPJBA
056201070801      * Reperisco dati job
056202070801     c                   exsr      DatiJob
061100030123
061101070801     C                   ENDSR
061200070801      *---------------------------------------------------------------*
061300070801      * Reperimento Dati del job (Utente/Operativi)                   *
061400070801      *---------------------------------------------------------------*
061500070801     c     DatiJob       BEGSR
061600070801      *
061700070801     c     *dtaara       define    §azute        azuteds
061800070801     c     *dtaara       define    §datiute      ddatiute
061900070801      *
062000070801     c                   in(E)     *dtaara
062100070801     c                   IF        %ERROR or RSUT = *blanks
062200070801     c                   clear                   Tibs34Ds
062300070801     c                   call      'TIBS34R'
062400070801     c                   parm                    Tibs34Ds
062500070801     c                   in        *dtaara
062600070801     c                   ENDIF
062700070801      *
062800070801     c                   ENDSR
