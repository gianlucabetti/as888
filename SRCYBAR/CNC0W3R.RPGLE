000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200020926     H OPTION(*NODEBUGIO)
000300900606?     *--------------------------------------------------------------*
000400030404      * CNC0W3R  controllo quadratura incassi giornalieri dei p.o.   *
000500991029?     *--------------------------------------------------------------*
000600060224     Fyfqig02l  iF   E           K DISK
000700030411     Fazorg01l  iF   E           K DISK
000800130926     Fazcae02l  iF   E           K DISK
000900130926      *----------
001000030404     D kpjba         e ds
001100031111     D tibs56ds      E DS
001200031111     d  lin                    7    756  0 dim(250)
001300130924     D*
001400030404     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
001500030404     D CNCR80        E DS
001600030404     D trul52ds      E DS
001700030411     D og148         E DS
001800030404     D com08           S              8                                         DS3A TIPO
001900030404     D dtaeur          S               d   datfmt(*eur) inz                     DS3A TIPO
002000030404     D dtaiso          S               d   inz                                  DS3A TIPO
002100130926
002200130930     d kcaeepa         s                   like(caeepa)
002300130930     d kcaetfA         s                   like(caetfA)
002400130930     d kcaetfP         s                   like(caetfP)
002500130930     d kcaedde         s                   like(caedde)
002600130930
002700130926     d datasys         s               d   inz(*sys) datfmt(*iso)
002800130926     d wdata           s              8  0
002900130926     d Salta_Filiale   s              1
003000991029?     *--------------------------------------------------------------*
003100991029      * CICLO PRINCIPALE
003200991029?     *--------------------------------------------------------------*
003300030404     c                   if        d52err = ' '
003400031111     c                   do        250           i                 3 0
003500030404      *loop con le chiavi delle tabelle personalizzate per filiale
003600030404     c                   if        lin(i) = 0
003700030404     c                   leave
003800030404     c                   endif
003900060224     c     kqig          chain     yfqig02l
004000060224     c                   if        not %found(yfqig02l)
004100030411     c* controllo che il po sia operativo
004200030411     c                   exsr      srorg
004300030411     c                   if        §oglpo = ' '
004400030411     c                   iter
004500030411     c                   end
004600130926     c*
004700140319     c*  Controlla che non sia in £6  e poi £j
004800130926
004900130926     c                   eval      kcaeepa = '6'
005000130930     c                   Z-ADD     lin(i)        kcaetfA
005100130930     c                   Z-ADD     0             kcaetfP
005200130930     c                   Z-ADD     Wdata         kcaeDDE
005300130930      *
005400130930     c     kcaetDDE      setGT     azcae02l
005500130930     c     kcaetA        readPE    azcae02l
005600130926     c*  se dentro la £6 deve saltare a leggerne un altra
005700130930     c                   if        not %EoF(azcae02l) and
005800130930     c                             caedde <= wdata and wdata <= caedsc
005900130930     c                   iter
006000130930     c                   end
006100130926      ***
006101140319     c                   eval      kcaeepa = 'J'
006102140319     c                   Z-ADD     lin(i)        kcaetfA
006103140319     c                   Z-ADD     0             kcaetfP
006104140319     c                   Z-ADD     Wdata         kcaeDDE
006105140319      *
006106140319     c     kcaetDDE      setGT     azcae02l
006107140319     c     kcaetA        readPE    azcae02l
006108140319     c*  se dentro la £6 deve saltare a leggerne un altra
006109140319     c                   if        not %EoF(azcae02l) and
006110140319     c                             caedde <= wdata and wdata <= caedsc
006111140319     c                   iter
006112140319     c                   end
006200130926     c*
006300060224     c     kqig          setll     yfqig02l
006400060224     c     lin(i)        readpe    yfqig02l
006500060224     c                   if        %eof(yfqig02l)
006600030404     c                   eval      com08 = '??-??-??'
006700030404     c                   else
006800030404     c                   move      qigdta        dtaiso
006900030404     c                   move      dtaiso        dtaeur
007000030404     c     *DMY          move      dtaeur        com08
007100030404     c                   end
007200030404     c                   exsr      srmsg
007300030404     c                   end
007400030404     c                   ENDDO
007500030404     c                   end
007600030404
007700030404     C                   eval      *inlr = *on
007800030404     C*-----------------------------------------------------------*
007900030404     C*  msg a cli* in sede di errata quadratura
008000030404     C*-----------------------------------------------------------*
008100030404     C     srmsg         BEGSR
008200100820     C                   eval      emogg  = 'QUADRATURA GIORNALIERA -
008300030404     C                             INCASSI E VERSAMENTI NON EFFETTUATA'
008400030404     c                   if        d52err = ' '
008500100820     C                   eval      emmsg  = 'Punto operativo '
008600030404     c                             + %editc(lin(i):'X')
008700030404     c                             + ' ultima chiusura '
008800030404     c                             + COM08
008900030404     c                   else
009000100820     C                   eval      emmsg  = 'Non reperito calendario aziendale'
009100030404     c                   end
009200120215     C* UTENTE  A CUI INVIARE la mail
009300130701     c                   eval      emlto = 'GestioneClienti@brt.it'
009400130701     c*                  eval      emlto = 'ufficio.pagamenti@bartolini.it'
009500100820     c                   eval      emlcc = *blank
009600100820     C                   CALL      'TIS701C1'
009700100820     C                   PARM                    emlto           253
009800100820     C                   PARM                    emlcc           253
009900100820     C                   PARM                    emogg            44
010000100820     C                   PARM                    emmsg          5000
010100100820     C                   PARM                    emesito           1
010200030404     C                   ENDSR
010300991102     C*-----------------------------------------------------------*
010400030411     C*  aggancio AZORG01L
010500991029     C*-----------------------------------------------------------*
010600030411     C     srorg         BEGSR
010700030411     c                   clear                   og148
010800030411     c     lin(i)        chain     azorg01l
010900030411     c                   if        %found(azorg01l)
011000030411     c                   eval      og148 = orgde8
011100030411     c                   end
011200030411     C                   ENDSR
011300030411     C*-----------------------------------------------------------*
011400030411     C*  OPERAZIONI INIZIALI
011500030411     C*-----------------------------------------------------------*
011600030411     C     *INZSR        BEGSR
011700991029     C*
011800030404     C                   Z-ADD     1             CODUT
011900030404     C                   CALL      'X§PARUT'
012000030404     C                   PARM                    UTEDSE
012100030404     C                   MOVEL     REC80         CNCR80
012200900515      *---------------------------------------------------------------*
012300900515      * CHIAVI                                                        *
012400900515      *---------------------------------------------------------------*
012500021119     C     Kqig          KLIST
012600030404     C                   KFLD                    lin(i)
012700030404     C                   KFLD                    D52DFI
012800130926
012900130930     c     kcaetA        klist
013000130926     c                   kfld                    kcaeepa
013100130930     c                   kfld                    kcaetfA
013200130930     c                   kfld                    kcaetfP
013300130930
013400130930     c     kcaetDDE      klist
013500130930     c                   kfld                    kcaeepa
013600130930     c                   kfld                    kcaetfA
013700130930     c                   kfld                    kcaetfP
013800130930     c                   kfld                    kcaedde
013900130926
014000031111     C* CARICO I P.O. dell'as
014100031111     c                   if        simfel>0
014200031111     c                   clear                   tibs56ds
014300031111     c                   eval      i56ppo=simfel
014400031111     c                   eval      i56mod='POA'
014500031111     c                   call      'TIBS56R'
014600031111     c                   parm                    tibs56ds
014700031111     c                   end
014800030404     c* reperisce data gg lavorativo precedente
014900030404     c                   clear                   trul52ds
015000030404     C                   MOVE      *date         dtaiso
015100030404     C                   MOVE      dtaiso        d52din
015200030404     c                   eval      d52gio = 1
015300030404     c                   eval      d52sgn = '-'
015400030404     c                   call      'TRUL52R'
015500030404     c                   parm                    trul52ds
015600030404     c                   if        d52err <> ' '
015700030404     c                   exsr      srmsg
015800030404     c                   END
015900130926     C*  imposta la UDATE
016000130926     c                   eval      wdata=%dec(datasys)
016100991102     C*
016200991102     C                   ENDSR
