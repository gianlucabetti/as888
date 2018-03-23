000100990504     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000200060125      *****************************************************************
000300170526      * Il pgm scrive i record di autisti con meno di 3 spedizioni pagati
000400170526      * a giornata
000500060125      *
000600060125      *****************************************************************
001100170526     Fwfiftt00f o    E             DISK
001101170529     Ffifgts0f  if   E           k DISK
001102170529     Ffiftt04l  if   E           k DISK    prefix(£)
001200030429     F*---------------------------------------------------------------------------------
001300000607     D kpjba         e ds
001400170526     D fifttds       E DS                  extname(fiftt00f)
001401170526     D ficnb3ds      E DS
001402170529     D WFTTDS          DS
001403170529     DT999G                           1
001404170529     DT999C                           1
001405170529     DT999R                           1
001406170529     DT000C                           1
001407170529     DT000R                           1
002500020205     c**********************************************************************
002501170529     C/EXEC SQL
002502170529     C+  DELETE WFIFTT00F
002503170529     C/END-EXEC
004800160126      *
004900170526     c* Lettura fifTT00F
005000160126     C/EXEC SQL
005100170526     C+ DECLARE A1 CURSOR FOR SELECT *
005200160126     C+  FROM fiftt00f WHERE
005300170526     C+  fttfvl = 'C' and fttset < 3 and ftttsr = ' '
005400170526     C+  and fttnff <> 0 and fttnff <> 999999999
005700160126     C/END-EXEC
005800020328
005900020328     C/EXEC SQL
006000020328     C+ OPEN A1
006100020328     C/END-EXEC
006200020328     C                   DOU       SqlCod <> 0
006300020328     C/EXEC SQL
006400170526     C+ FETCH NEXT FROM A1 INTO :fifttds
006500020328     C/END-EXEC
006600020328     C                   SELECT
006700060125     c*
006800020328     C                   WHEN      SqlCod = 0
006900170526     c                   exsr      scrivi
007000060125     C*
007100060125      **
007200060125      * ? EoF
007300020328     C                   WHEN      SqlCod = 100
007400160126     c                   leave
007500060125      **
007600060125      * ? Errori
007700020328     C                   WHEN      SqlCod <> 0
007800160126     c                   leave
007900060125      **
008000020328     C                   ENDSL
008100060125      **
008200020328     C                   ENDDO
008300020328     C/EXEC SQL
008400020328     C+ CLOSE A1
008500020328     C/END-EXEC
008700030429      *
008800000607     c                   seton                                        lr
008900160125     C**********************************************************************
009000170526     C** scrittura file
009100160125     C**********************************************************************
009200170526     C     scrivi        BEGSR
009201170526     C*
009202170526     C* Routine esterna per restituire il codice tariffa del documento
009203170529     c                   clear                   WFTTDS
009206170529     C                   clear                   ficnb3ds
009207170529     C                   movel     fttpdr        §t3pdr
009208170529     C                   movel     ' '           §t3sml
009209170529     C                   z-add     fttddc        §t3ddc
009210170529     C                   movel     'EUR'         §t3div
009211170529     C                   movel     'C'           §t3fvl
009212170529     c                   do        5             i                 2 0
009220170529     c                   select
009221170529     c                   when      i = 1
009222170529     C                   movel     '999'         §t3ctr
009223170529     c                   movel     'G'           §t3tsr
009225170529     c                   when      i = 2
009226170529     C                   movel     '999'         §t3ctr
009227170526     C                   movel     'R'           §t3tsr
009228170529     c                   when      i = 3
009229170529     C                   movel     '999'         §t3ctr
009230170526     C                   movel     'C'           §t3tsr
009231170529     c                   when      i = 4
009232170529     C                   CLEAR                   §t3ctr
009233170529     C                   movel     'R'           §t3tsr
009234170529     c                   when      i = 5
009235170529     C                   CLEAR                   §t3ctr
009236170529     C                   movel     'C'           §t3tsr
009237170526     c                   endsl
009238170529     C                   CALL      'FICNB3R1'
009239170529     C                   PARM                    ficnb3ds
009240170526     c                   select
009241170529     c* se non presente la tariffa a giornata, non devo scrivere il record
009242170529     c                   when      i = 1 and §t3ctr <> '???'
009243170529     c                   EVAL      T999G = *ON
009244170529     c                   when      i = 2 and §t3ctr <> '???'
009245170529     c                   eval      T999R = *on
009246170529     c                   when      i = 3 and §t3ctr <> '???'
009247170529     c                   eval      T999C = *on
009248170529     c                   when      i = 4 and §t3ctr <> '???'
009249170529     c                   eval      T000R = *on
009250170529     c                   when      i = 5 and §t3ctr <> '???'
009251170529     c                   eval      T000C = *on
009252170526     c                   endsl
009253170526     c                   enddo
009257170529     c* tariffa scaduta
009258170529     c                   if        WFTTDS = ' '
009259170529     c                   exsr      tarscad
009261170529     c                   end
009262170529     C                   MOVEL(P)  WFTTDS        FTTFLR2
011700170526     c                   write     fifttw00
014900160125     C                   endsr
016300020205     C**-------------------------------------------------------------**
016400170529     c     tarscad       begsr
016500060125     C**-------------------------------------------------------------**
016501170529     C                   CLEAR                   FGTSML
016502170529     c     kfgts         setll     fifgts0f
016503170529     c                   do        *hival
016504170529     c     kfgts         reade     fifgts0f
016505170529     c                   if        %eof(fifgts0f)
016506170529     c                   leave
016507170529     c                   end
016508170529     c                   if        fgttsr <> 'G' and fgttsr <> 'C' and
016509170529     c                             fgttsr <> 'R'
016510170529     c                   iter
016511170529     c                   end
016512170529     c                   if        fgtDTS = 0
016513170529     c                   iter
016514170529     c                   end
016515170529     c                   if        fgtddt <= fttddc and fgtdst >=
016516170529     c                             fttddc
016517170529     c                   if        fgtctr = 999
016518170529     c                   select
016519170529     c                   when      fgttsr = 'G'
016520170529     c                   eval      t999G = *on
016521170529     c                   when      fgttsr = 'C'
016522170529     c     kftt04        setll     fiftt04l
016523170529     c                   if        %equal(fiftt04l)
016524170529     c                   eval      t999C = *on
016525170529     c                   end
016526170529     c                   when      fgttsr = 'R'
016527170529     c     kftt04        setll     fiftt04l
016528170529     c                   if        %equal(fiftt04l)
016529170529     c                   eval      t999R = *on
016530170529     c                   end
016531170529     C                   ENDSL
016532170529     c                   else
016533170529     c                   select
016534170529     c                   when      fgttsr = 'C'
016535170529     c     kftt04        setll     fiftt04l
016536170529     c                   if        %equal(fiftt04l)
016537170529     c                   eval      t000C = *on
016538170529     c                   end
016539170529     c                   when      fgttsr = 'R'
016540170529     c     kftt04        setll     fiftt04l
016541170529     c                   if        %equal(fiftt04l)
016542170529     c                   eval      t000R = *on
016543170529     c                   end
016544170529     C                   ENDSL
016545170529     c                   end
016546170529     c                   end
016547170529     c                   enddo
016548170529     C                   endsr
016549170529     C**-------------------------------------------------------------**
016550170529     c     *inzsr        begsr
016551170529     C**-------------------------------------------------------------**
016600000607     c     *entry        plist
016700020205     c                   parm                    kpjba
016701170529     C     KFGTS         KLIST
016702170529     C                   KFLD                    FTTPDR
016703170529     C                   KFLD                    FGTSML
016704170529     C     KFtt04        KLIST
016705170529     C                   KFLD                    FTTfvl
016706170529     C                   KFLD                    Fttddc
016707170529     C                   KFLD                    Fttpdr
016708170529     C                   KFLD                    Fgttsr
016709170529     c                   eval      fttfvl = 'C'
018800020402     c                   endsr
