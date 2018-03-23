000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300060127      * Il pgm deve isolare le testate padroncini
000500060125      *
000501060127      * ?SELECT * from FIFTT00F where fttFTR = ' '
000900060125      *
001000060127      *  ossia tutti i conteggi non trasmessi a sede (poichè i files di
001100060127      *  sede in GAITRAGRU vengono spostati in UNITRAGRU)
001200060127      *
001300060127      *  occorre quindi accodare le testate rimaste in Filiale con tutti
001400060127      *  i records correlati di FTD, FCE, FRE
001500060125      *****************************************************************
002500060127     fWxftt00f  o  a e           k disk    rename(fiftt000:WXFTT000)
002501060127     fWxftd00f  o  a e           k disk    rename(fiftd000:WXFTD000)
002502060127     fWxfce00f  o  a e           k disk    rename(fifce000:WXFCE000)
002503060127     fWxfre00f  o  a e           k disk    rename(fifre000:WXFRE000)
003000030429     F*---------------------------------------------------------------------------------
003302060127     ffiftd01l  if   e           k disk    rename(fiftd000:ftd01)
003303060127     ffifce01l  if   e           k disk    rename(fifce000:fce01)
003304060127     ffifre02l  if   e           k disk    rename(fifre000:fre02)
003400030429     F*---------------------------------------------------------------------------------
004601060125     D fiftt         E DS                  extname(fiftt00f)
006105060125      *
006800020328     D WrkSqlCmd       S           1024
007201060125      *
007202060127     D savpdr          S                   like(fttpdr)
007203060127     D savtsr          S                   like(ftttsr)
007204060127     D savddc          S                   like(fttddc)
007205060127      *
007800020205     c**********************************************************************
007802060127     c* MI COMPONGO L'ISTRUZIONE SQL
007803060127     c                   EVAL      WRKSQLCMD = 'SELECT * FROM ' +
007804060127     c                             FILLIB
007805060127     c                   EVAL      WRKSQLCMD = %TrimR(WRKSQLCMD) +
007806060127     c                             '/FIFTT00F WHERE FTTFTR = '' '' +
007807060127     c                             ORDER BY FTTPDR, FTTTSR, FTTDDC, FTTNDC'
007808060127     c**********************************************************************
007809060127     c*
007900060127     c* Lettura fifTT00F
008000020328     C/EXEC SQL
008100020328     C+ PREPARE S1 FROM :WrkSqlCmd
008200020328     C/END-EXEC
008300020328
008400020328     C/EXEC SQL
008500020328     C+ DECLARE A1 CURSOR FOR S1
008600020328     C/END-EXEC
008700020328
008800020328     C/EXEC SQL
008900020328     C+ OPEN A1
009000020328     C/END-EXEC
009200020328     C                   DOU       SqlCod <> 0
009300020328     C/EXEC SQL
009400060127     C+ FETCH NEXT FROM A1 INTO :fiftt
009500020328     C/END-EXEC
009700020328     C                   SELECT
009701060125     c*
009800020328     C                   WHEN      SqlCod = 0
009801060127      * ? Scrive i 4 archivi
009802060127      * ?      Testate
009803060127     c                   write     wxftt000
009804060127      *
009805060127      *  x testate non di TOTALE
009806060127     c                   if        FTTNDC > 0
010201060125     c*
010203060127      * ?      Dettagli  x PDR/TSR/NDC/DDC
010205060127     C     Kftd          setll     Fiftd01l
010206060127     C     Kftd          reade     Fiftd01l
010207060127     c                   dow       not %EoF(Fiftd01l)
010208060127     c                   write     wxftd000
010234060127      *
010235060127     C     Kftd          reade     Fiftd01l
010236060127     c                   EndDO
010237060127      *
010239060127      * ?      Costi     x FGS/PDR/TSR/NDC/DDC
010240060127     c                   movel     fttpdr        fcefgs
010241060127     C     Kfce          setll     Fifce01l
010242060127     C     Kfce          reade     Fifce01l
010243060127     c                   dow       not %EoF(Fifce01l)
010244060127     c                   write     wxfce000
010245060127     C     Kfce          reade     Fifce01l
010246060127     c                   EndDO
010247060127      *
010248060127      * ?      Rettifiche x TIP/PDR/DDC/TSR
010251060127     /*?    Attenzione la chiave può ripetersi  e se già scritto in      */
010252060127     /*?    WXFRE00F non deve essere aggiunto nuovamente.                */
010253060130     c                   if        fttpdr <> savpdr or
010254060130     c                             ftttsr <> savtsr or
010255060127     c                             fttddc <> savddc
010256060127     c                   move      fttpdr        savpdr
010257060127     c                   move      ftttsr        savtsr
010258060127     c                   move      fttddc        savddc
010259060127     c                   eval      fretip = 'A'
010260060127     C     Kfre          setll     Fifre02l
010261060127     C     Kfre          reade     Fifre02l
010262060127     c                   dow       not %EoF(Fifre02l)
010263060127     c                   write     wxfre000
010264060127     C     Kfre          reade     Fifre02l
010265060127     c                   EndDO
010266060127     c                   endIF
010267060127      *
010268060127     c                   end
011401060125      * ?
012602060125      * ? EoF
012700020328     C                   WHEN      SqlCod = 100
012701060125      **
012702060125      * ? Errori
012800020328     C                   WHEN      SqlCod <> 0
012801060125      **
012900020328     C                   ENDSL
012901060125      **
013000020328     C                   ENDDO
013200020328     C/EXEC SQL
013300020328     C+ CLOSE A1
013400020328     C/END-EXEC
015000030429      *
015100000607     c                   seton                                        lr
031500020205     C**-------------------------------------------------------------**
034900000607     c     *inzsr        begsr
034901060125     C**-------------------------------------------------------------**
035200000607     c     *entry        plist
035300060127     c                   parm                    UNILIB           10
035301060127     c                   parm                    GRULIB           10
035302060127     c                   parm                    FILLIB           10
035303060127      *----------*
035304060127     c     kftd          klist
035305060127     c                   kfld                    fttpdr
035306060127     c                   kfld                    ftttsr
035307060127     c                   kfld                    fttndc
035308060127     c                   kfld                    fttddc
037600030429      *----------*
040600060127     c     kfce          klist
040601060127     c                   kfld                    FceFGS
040700060127     c                   kfld                    FttPDR
040701060127     c                   kfld                    FttTSR
040703060127     c                   kfld                    FttNDC
040704060127     c                   kfld                    FttDDC
040705060127      *----------*
040706060127     c     kfre          klist
040707060127     c                   kfld                    fretip
040708060127     c                   kfld                    fttpdr
040710060127     c                   kfld                    fttddc
040711060127     c                   kfld                    ftttsr
041001060127      *----------*
041400020402     c                   endsr
043804060125     O*------------------------------------------------------*
