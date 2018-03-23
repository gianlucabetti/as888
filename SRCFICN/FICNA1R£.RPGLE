000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200070111     H* FICNA1R *----------------------------------------------------*
000300000000     H*-----------------*                                            *
000400070111     H*         -  ELIMINA CONTEGGI AUTISTI                          *
000500000000     H*--------------------------------------------------------------*
000600070111     FficnA1D   CF   E             WORKSTN
000700070112     Ffiapd01L  iF   E           K DISK
000800070112     Ffifre02L  iF   E           K DISK
000900070112      *
001000070112     Ffiftt01L  iF   E           K DISK
001100070112     Ffiftt02L  uF   E           K DISK    rename(FIFTT000:FIFTT02)
001200070111     Ffiftd05L  UF   E           K DISK
001300070112     Ffifce01L  iF a E           K DISK
001400070112     H*--------------------------------------------------------------*
001500930707     F*
001600931126     D WLBDAT          DS
001700950120     D  G02DAT                 1      8  0
001800950120     D  G02INV                 9     16  0
001900950120     D  G02ERR                17     17
002000950120     D  G02TGI                18     22  0
002100070112      *
002200931126     D WGIDAT          DS
002300950120     D  GIODAT                 1      8  0
002400950120     D  GIOINV                 9     16  0
002500950120     D  GIOTGI                17     21  0
002600070112      *
002700141104     D ficnB30DS     E DS
002800131003     D tibs42ds      E DS
002900131003     D uteautds      E DS
003000070111     D CNCR80        E DS
003100070111     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003200091103     D* DS PER TRUL06R - CARICAMENTO £X
003300091103     D DSUL06        E DS                  EXTNAME(TRUL06DS)
003400091103     D  LIN                    1     90  0
003500091103     D                                     DIM(30)                              FIL. COMODO
003600091103     D L1              S              3  0 DIM(30)                              FIL GESTITE
003700920810     D KPJBA         E DS
003800900521     C*****************************************************************
003900091103     C*
004000070112     c                   move      'S'           vidCMP
004100070111     C*
004200070111     C     FOR01         TAG
004300070111     c                   setoff                                       99
004400070111     C                   EXFMT     ficna01
004500091103     c   33              move      'S'           vidCMP
004600091103     c   33              move      VIDAU1        VIDAU2
004700091103     c   33              move      '0998'        VIDAU2
004800130808     C*  Sotto autorizzazione solo chi valorizza può utilizzarlo oppure EDP
004900130808     c                   if        %subst(knmus:1:3) <>'EDP'
005000130808     C*
005100130808     C* Controllo autorizzazione all'utilizzo del programma
005200130808     c* Controllo se abilitato
005300131003     c                   clear                   tibs42ds
005400131003     c                   movel     vidau1        i42pge
005500131003     c                   call      'TIBS42R'
005600131003     c                   parm                    tibs42ds
005700131003     c                   movel     o42uni        uteautds
005800130808     c* 98 indicatore che protegge tutto
005900130808     c     §autcon       ifne      'S'
006000130808     C                   exfmt     NONAUT
006100130808     C                   GOTO      FINE
006200130808     C                   endif
006300130808      *
006400130808     c                   end
006500070111     C** CMD3 -
006600070111     C   KC              GOTO      FINE
006700070111     C* CONTROLLI
006800070111     C                   EXSR      CONTR1
006900070111     C  NKF
007000070111     COR 99              GOTO      FOR01
007100070111     C*
007200070111     C                   EXSR      aggiorna
007300070111     C*
007400070111     C     FINE          TAG
007500070111     C                   SETON                                        LR
007600070111     C*---------------------------------------------------------------*
007700070112     C*- - CONTROLLI video    -                                      -*
007800070112     C*---------------------------------------------------------------*
007900070112     C     CONTR1        BEGSR
008000070112     C*
008100070112     C                   SETOFF                                       99
008200070112     C* Autista Conteggi
008300070112     C                   clear                   VIDDA1
008400070112     C     kapd          CHAIN     fiapd01L
008500070112     C                   IF        not %Found(fiapd01L)
008600070112     C                   SETON                                        2299
008700070112     C                   GOTO      ENDCT1
008800070112    2C                   END
008900100223     C                   MOVEL     APDRSf        VIDDA1
009000070112     C*
009100070112     C* D A T A   D I S T I N T A
009200070112     C                   MOVE      VIDDDC        G02DAT
009300070112     C                   MOVEL     *BLANK        G02ERR
009400070112     C                   CALL      'XSRDA8'
009500070112     C                   PARM                    WLBDAT
009600070112    2C     G02ERR        IFEQ      '1'
009700070112     C                   SETON                                        2399
009800070112     C                   GOTO      ENDCT1
009900070112    2C                   END
010000070112     C                   Z-ADD     G02INV        W01DDC
010100070112     C                   Z-ADD     G02DAT        VIDDDC
010200070112     C*
010300070112     C* CONTROLLO SE ESISTE LA SELEZIONE
010400070112     C     KFTT          chain(n)  fiftt02L
010500070112     c                   if        not %Found(fiftt02L)
010600070112     C                   SETON                                        2699
010700070112     C                   GOTO      ENDCT1
010800070112     c                   end
010900070112      *
011000070112     C* CONTROLLO SE ESISTE LA SELEZIONE del documento se immesso
011100070112     c                   if        vidndc > 0
011200070112     c                   move      'C'           W01TSR
011300070112     C     KFTT01        chain     fiftt01L
011400070112     c                   if        not %Found(fiftt01L)
011500070112     c                   move      'R'           W01TSR
011600070112     C     KFTT01        chain     fiftt01L
011700070112     c                   if        not %Found(fiftt01L)
011800070112     C                   SETON                                        262799
011900070112     C                   GOTO      ENDCT1
012000070112     c                   end
012100070112     c                   end
012200070112      *
012300070112      * Ci sono delle rettifiche sul Padroncino da eliminare alla data
012400070112     C     KFRE          chain     fiFRE02l
012500070112     c                   if        %Found(fiFRE02l)
012600070112     C                   SETON                                        2899
012700070112     C                   GOTO      ENDCT1
012800070112     c                   end
012900070112     C*
013000070112     c                   else
013100070112      *
013200070112      * Ci sono delle rettifiche sul Padroncino da eliminare alla data
013300070112     C     KFRE1         chain     fiFRE02l
013400070112     c                   if        %Found(fiFRE02l)
013500070112     C                   SETON                                        2899
013600070112     C                   GOTO      ENDCT1
013700070112     c                   end
013800070112     C*
013900070112     c                   end
014000070112     C*
014100070112     C* Autista x competenze
014200070112     C                   clear                   VIDDA2
014300070112     C*
014400070718     c                   if        vidcmp = 'N' and vidau2 > 0
014500070718     C                   SETON                                        2499
014600070112     C                   GOTO      ENDCT1
014700070718     c                   elseIF    vidau2 = 0 and vidcmp = 'S'
014800070718     C                   SETON                                        2599
014900070718     C                   GOTO      ENDCT1
015000070718     c                   end
015100070112     C*
015200070718     c                   if        vidau2 > 0
015300070112     C     kapd1         CHAIN     fiapd01L
015400070112     C                   IF        not %Found(fiapd01L)
015500070112     C                   SETON                                        2599
015600070112     C                   GOTO      ENDCT1
015700070718    2C                   Elseif    APDPDD <> 'S'
015800070718     C                   SETON                                        2999
015900070718     C                   GOTO      ENDCT1
016000070718    2C                   END
016100100223     C                   MOVEL     APDRSf        VIDDA2
016200070112     c                   end
016300141104      *
016400141104     **  ulteriore controllo sull'autista e le distinte alla data.
016500141104     C* CONTROLLO SE CI SONO DISTINTE APERTE, BOLLE NON PARTITE, CONTEGGI
016600141104     C* NON VALORIZZATI SOLO PER AUT DI CITTÀ
016700141104     C                   clear                   ficnb30ds
016800141104     c* non controllo la massa perchè lo faccio dopo per ogni fornitore che
016900141104     c* che autofattura
017000141104     c                   eval      Ctrmassa = 'N'
017100141105     c* deve controllare che siano tutte Svalorizzate
017200141105     c                   eval      TIPO_VALOR=*blank
017300141104     c                   z-add     VIDAU1        CODAUT_DA
017400141104     c                   z-add     VIDAU1        CODAUT_A
017500141104     c                   movel     VIDAU1        CODFILIALE
017600141104     c                   z-add     W01DDC        DATA_DOCUM
017700141104     c                   call      'FICNB30R'
017800141104     c                   parm                    ficnb30ds
017900141105     c                   if        TIPOERRORE =  'C'
018000141105     C                   SETON                                        4199
018100141105     C                   GOTO      ENDCT1
018200141105     c                   elseIf    TIPOERRORE =  'P'
018300141105     C                   SETON                                        4299
018400141105     C                   GOTO      ENDCT1
018500141105     c                   elseIf    TIPOERRORE =  'V'
018600141105     C                   SETON                                        4399
018700141105     C                   GOTO      ENDCT1
018800141104     c                   end
018900141104      *
019000141104      *
019100070112     C     ENDCT1        ENDSR
019200070112     C*---------------------------------------------------------------*
019300070112     C*   Aggiorna gli archivi
019400070112     C*---------------------------------------------------------------*
019500070112     C     Aggiorna      BegSR
019600070112     C*
019700070112     C     kFTT          setll     Fiftt02L
019800070112     C     kFTT          reade     Fiftt02L
019900070112     c                   dow       not %EoF(Fiftt02l)
020000070112     C*
020100070112     C* Se mirato sul documento
020200070112     c                   if        vidndc > 0  and vidndc <> fttndc
020300070112     c                   goto      salta
020400070112     c                   end
020500070112     C*
020600070112     C*  prima di cancellare deve impostare FIFCE x generare la spia
020700070112     C*   delle competenze
020800070112     c     kFTD          setll     Fiftd05l
020900070112     c     kFTD          Reade     Fiftd05l
021000070112      *
021100070112     c                   dow       not %EoF(Fiftd05l)
021200070112      *
021300070112      * genera FCE x non perdere le competenze
021400070112     c                   if        vidCMP ='S'
021500070112     c                   exsr      write_FCE
021600070112     c                   end
021700070112      *
021800070112     c                   delete    fiFTD000
021900070112      *
022000070112     c     kFTD          Reade     Fiftd05l
022100070112     c                   enddo
022200070111     C*
022300070112     c                   delete    fiFTT02
022400070112     C*
022500070112     c     salta         tag
022600070112     C*
022700070112     C     kFTT          reade     Fiftt02L
022800070112     c                   enddo
022900070112     C*
023000070112     C                   EndSR
023100070112     C*---------------------------------------------------------------*
023200070112     C     write_FCE     BegSR
023300070112     C*
023400070112     c                   if        ftdtsr = 'C'
023500070112     c                   move      '005'         w01CCE
023600070112     c                   end
023700070112     C*
023800070112     c                   if        ftdtsr = 'R'
023900070112     c                   move      '001'         w01CCE
024000070112     c                   end
024100070112     C*
024200070112     C*  se il codice Autista Competenze da attribuire è impostato o no
024300070112     c                   if        vidAU2 > 0
024400070112     c                   eval      w01PDR = vidAU2
024500070112     c                   else
024600070112     c                   eval      w01PDR = FtdPDR
024700070112     c                   end
024800070112     C*
024900070112     C     kFCE          chain     FiFCE01l
025000070112     c                   if        not %Found(FiFCE01l)
025100070112     C                   clear                   fiFCE000
025200070112     c                   eval      FCEPDR =  w01PDR
025300070112     c                   eval      FCETSR =  FtdTSR
025400070112     c                   eval      FCEFGS =  FtdFGS
025500070112     c                   eval      FCENDC =  FtdNDC
025600070112     c                   eval      FCEDDC =  FtdDDC
025700070112     c                   eval      FCEAAS =  FtdAAS
025800070112     c                   eval      FCELNP =  FtdLNP
025900070112     c                   eval      FCENRS =  FtdNRS
026000070112     c                   eval      FCENSP =  FtdNSP
026100070112     c                   eval      FCECCE =  w01CCE
026200070112
026300070112     C                   write     fiFCE000
026400070112     C                   End
026500070112     C*
026600070112     C                   EndSR
026700070112     C*****************************************************************
026800070112     C     *inzsr        BegSR
026900070112      *
027000070112     C     *ENTRY        PLIST
027100070112     C                   PARM                    KPJBA
027200070112      *
027300091103     c                   seton                                        33
027400091103     c                   if        %subst(knmus:1:3) = 'EDP'
027500091103     c                   setoff                                       33
027600091103     c                   end
027700091103      *
027800070112     C                   Z-ADD     1             CODUT
027900070112     C                   CALL      'X§PARUT'
028000070112     C                   PARM                    UT§DSE
028100070112     C                   MOVEL     RAGUT         vidrsu
028200070112     C                   MOVEL     REC80         CNCR80
028300091103     C***
028400091103     C* CARICO TABELLA FILIALI GESTITE £1
028500091103     C***
028600091103     C                   CLEAR                   DSUL06
028700091103     C                   MOVE      '£1'          D06COD
028800091103     C                   MOVEL     SIMFEL        D06KEY
028900091103     C                   MOVEL     ' '           D06TLA
029000091103     C                   MOVEL     DSUL06        KPJBU
029100091103     C                   CALL      'TRUL06R'
029200091103     C                   PARM                    KPJBA
029300091103     C                   MOVEL     KPJBU         DSUL06
029400091103     C                   MOVEA     LIN           L1
029500091103     C***
029600070112     C*---------------------------------------------------------------*
029700070112     C     kFTT          KLIST
029800070112     C                   KFLD                    VIDAU1
029900070112     C                   KFLD                    W01FVL
030000070112     C                   KFLD                    W01DDC
030100070112      *
030200070112     C     kFTT01        KLIST
030300070112     C                   KFLD                    VIDAU1
030400070112     C                   KFLD                    W01TSR
030500070112     C                   KFLD                    VIDNDC
030600070112     C                   KFLD                    W01DDC
030700070112      *
030800070112     C     kFTD          KLIST
030900070112     C                   KFLD                    FTTPDR
031000070112     C                   KFLD                    FTTTSR
031100070112     C                   KFLD                    FTTNDC
031200070112     C                   KFLD                    FTTDDC
031300070112     C*
031400070112     C     kFCE          KLIST
031500070112     C                   KFLD                    FtdFGS
031600070112     C                   KFLD                    w01PDR
031700070112     C                   KFLD                    FtdTSR
031800070112     C                   KFLD                    FtdNDC
031900070112     C                   KFLD                    FtdDDC
032000070112     C                   KFLD                    FtdAAS
032100070112     C                   KFLD                    FtdLNP
032200070112     C                   KFLD                    FtdNRS
032300070112     C                   KFLD                    FtdNSP
032400070112     C                   KFLD                    w01CCE
032500070112     C*
032600070112     C     kFRE          KLIST
032700070112     C                   KFLD                    Ktip              1
032800070112     C                   KFLD                    VIDAU1
032900070112     C                   KFLD                    W01DDC
033000070112     C                   KFLD                    W01TSR
033100070112     C*
033200070112     C     kFRE1         KLIST
033300070112     C                   KFLD                    Ktip              1
033400070112     C                   KFLD                    VIDAU1
033500070112     C                   KFLD                    W01DDC
033600070112     C*
033700070112     C     kapd          KLIST
033800070112     C                   KFLD                    Ktip              1
033900070112     C                   KFLD                    VIDAU1
034000070112     C*
034100070112     C     kapd1         KLIST
034200070112     C                   KFLD                    Ktip              1
034300070112     C                   KFLD                    VIDAU2
034400070112      *
034500070112     C                   move      *blank        W01FVL
034600070112     C                   move      'A'           Ktip              1
034700070112     C*
034800070112     C* DEFINIZIONE CAMPI
034900070112     C     *LIKE         DEFINE    FTTDDC        W01DDC
035000070112     C     *LIKE         DEFINE    FTTTSR        W01TSR
035100070112     C     *LIKE         DEFINE    FTTFVL        W01FVL
035200070112     C     *LIKE         DEFINE    FCECCE        W01CCE
035300070112     C     *LIKE         DEFINE    FCEPDR        W01PDR
035400070112     C*
035500070112     C* LA DATA DEL GIORNO LA PRENDO DA TIME
035600070112     C                   TIME                    W0140            14 0
035700070112     C                   MOVE      W0140         UDATE8            8 0
035800070112     C*
035900070112     C                   EndSR
036000070112     C*---------------------------------------------------------------*
