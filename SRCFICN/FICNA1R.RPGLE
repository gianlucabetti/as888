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
004800130808     C*
004900130808     C* Controllo autorizzazione all'utilizzo del programma
005000130808     c* Controllo se abilitato
005100131003     c                   clear                   tibs42ds
005200131003     c                   movel     vidau1        i42pge
005300131003     c                   call      'TIBS42R'
005400131003     c                   parm                    tibs42ds
005500131003     c                   movel     o42uni        uteautds
005600130808     c* 98 indicatore che protegge tutto
005700130808     c     §autcon       ifne      'S'
005800130808     C                   exfmt     NONAUT
005900130808     C                   GOTO      FINE
006000130808     C                   endif
006100130808      *
006200070111     C** CMD3 -
006300070111     C   KC              GOTO      FINE
006400070111     C* CONTROLLI
006500070111     C                   EXSR      CONTR1
006600070111     C  NKF
006700070111     COR 99              GOTO      FOR01
006800070111     C*
006900070111     C                   EXSR      aggiorna
007000070111     C*
007100070111     C     FINE          TAG
007200070111     C                   SETON                                        LR
007300070111     C*---------------------------------------------------------------*
007400070112     C*- - CONTROLLI video    -                                      -*
007500070112     C*---------------------------------------------------------------*
007600070112     C     CONTR1        BEGSR
007700070112     C*
007800070112     C                   SETOFF                                       99
007900070112     C* Autista Conteggi
008000070112     C                   clear                   VIDDA1
008100070112     C     kapd          CHAIN     fiapd01L
008200070112     C                   IF        not %Found(fiapd01L)
008300070112     C                   SETON                                        2299
008400070112     C                   GOTO      ENDCT1
008500070112    2C                   END
008600100223     C                   MOVEL     APDRSf        VIDDA1
008700070112     C*
008800070112     C* D A T A   D I S T I N T A
008900070112     C                   MOVE      VIDDDC        G02DAT
009000070112     C                   MOVEL     *BLANK        G02ERR
009100070112     C                   CALL      'XSRDA8'
009200070112     C                   PARM                    WLBDAT
009300070112    2C     G02ERR        IFEQ      '1'
009400070112     C                   SETON                                        2399
009500070112     C                   GOTO      ENDCT1
009600070112    2C                   END
009700070112     C                   Z-ADD     G02INV        W01DDC
009800070112     C                   Z-ADD     G02DAT        VIDDDC
009900070112     C*
010000070112     C* CONTROLLO SE ESISTE LA SELEZIONE
010100070112     C     KFTT          chain(n)  fiftt02L
010200070112     c                   if        not %Found(fiftt02L)
010300070112     C                   SETON                                        2699
010400070112     C                   GOTO      ENDCT1
010500070112     c                   end
010600070112      *
010700070112     C* CONTROLLO SE ESISTE LA SELEZIONE del documento se immesso
010800070112     c                   if        vidndc > 0
010900070112     c                   move      'C'           W01TSR
011000070112     C     KFTT01        chain     fiftt01L
011100070112     c                   if        not %Found(fiftt01L)
011200070112     c                   move      'R'           W01TSR
011300070112     C     KFTT01        chain     fiftt01L
011400070112     c                   if        not %Found(fiftt01L)
011500070112     C                   SETON                                        262799
011600070112     C                   GOTO      ENDCT1
011700070112     c                   end
011800070112     c                   end
011900070112      *
012000070112      * Ci sono delle rettifiche sul Padroncino da eliminare alla data
012100070112     C     KFRE          chain     fiFRE02l
012200070112     c                   if        %Found(fiFRE02l)
012300070112     C                   SETON                                        2899
012400070112     C                   GOTO      ENDCT1
012500070112     c                   end
012600070112     C*
012700070112     c                   else
012800070112      *
012900070112      * Ci sono delle rettifiche sul Padroncino da eliminare alla data
013000070112     C     KFRE1         chain     fiFRE02l
013100070112     c                   if        %Found(fiFRE02l)
013200070112     C                   SETON                                        2899
013300070112     C                   GOTO      ENDCT1
013400070112     c                   end
013500070112     C*
013600070112     c                   end
013700070112     C*
013800070112     C* Autista x competenze
013900070112     C                   clear                   VIDDA2
014000070112     C*
014100070718     c                   if        vidcmp = 'N' and vidau2 > 0
014200070718     C                   SETON                                        2499
014300070112     C                   GOTO      ENDCT1
014400070718     c                   elseIF    vidau2 = 0 and vidcmp = 'S'
014500070718     C                   SETON                                        2599
014600070718     C                   GOTO      ENDCT1
014700070718     c                   end
014800070112     C*
014900070718     c                   if        vidau2 > 0
015000070112     C     kapd1         CHAIN     fiapd01L
015100070112     C                   IF        not %Found(fiapd01L)
015200070112     C                   SETON                                        2599
015300070112     C                   GOTO      ENDCT1
015400070718    2C                   Elseif    APDPDD <> 'S'
015500070718     C                   SETON                                        2999
015600070718     C                   GOTO      ENDCT1
015700070718    2C                   END
015800100223     C                   MOVEL     APDRSf        VIDDA2
015900070112     c                   end
016000141104      *
016100141104     **  ulteriore controllo sull'autista e le distinte alla data.
016200141104     C* CONTROLLO SE CI SONO DISTINTE APERTE, BOLLE NON PARTITE, CONTEGGI
016300141104     C* NON VALORIZZATI SOLO PER AUT DI CITTÀ
016400141104     C                   clear                   ficnb30ds
016500141104     c* non controllo la massa perchè lo faccio dopo per ogni fornitore che
016600141104     c* che autofattura
016700141104     c                   eval      Ctrmassa = 'N'
016800141105     c* deve controllare che siano tutte Svalorizzate
016900141105     c                   eval      TIPO_VALOR=*blank
017000141104     c                   z-add     VIDAU1        CODAUT_DA
017100141104     c                   z-add     VIDAU1        CODAUT_A
017200141104     c                   movel     VIDAU1        CODFILIALE
017300141104     c                   z-add     W01DDC        DATA_DOCUM
017400141104     c                   call      'FICNB30R'
017500141104     c                   parm                    ficnb30ds
017600141105     c                   if        TIPOERRORE =  'C'
017700141105     C                   SETON                                        4199
017800141105     C                   GOTO      ENDCT1
017900141105     c                   elseIf    TIPOERRORE =  'P'
018000141105     C                   SETON                                        4299
018100141105     C                   GOTO      ENDCT1
018200141105     c                   elseIf    TIPOERRORE =  'V'
018300141105     C                   SETON                                        4399
018400141105     C                   GOTO      ENDCT1
018500141104     c                   end
018600141104      *
018700141104      *
018800070112     C     ENDCT1        ENDSR
018900070112     C*---------------------------------------------------------------*
019000070112     C*   Aggiorna gli archivi
019100070112     C*---------------------------------------------------------------*
019200070112     C     Aggiorna      BegSR
019300070112     C*
019400070112     C     kFTT          setll     Fiftt02L
019500070112     C     kFTT          reade     Fiftt02L
019600070112     c                   dow       not %EoF(Fiftt02l)
019700070112     C*
019800070112     C* Se mirato sul documento
019900070112     c                   if        vidndc > 0  and vidndc <> fttndc
020000070112     c                   goto      salta
020100070112     c                   end
020200070112     C*
020300070112     C*  prima di cancellare deve impostare FIFCE x generare la spia
020400070112     C*   delle competenze
020500070112     c     kFTD          setll     Fiftd05l
020600070112     c     kFTD          Reade     Fiftd05l
020700070112      *
020800070112     c                   dow       not %EoF(Fiftd05l)
020900070112      *
021000070112      * genera FCE x non perdere le competenze
021100070112     c                   if        vidCMP ='S'
021200070112     c                   exsr      write_FCE
021300070112     c                   end
021400070112      *
021500070112     c                   delete    fiFTD000
021600070112      *
021700070112     c     kFTD          Reade     Fiftd05l
021800070112     c                   enddo
021900070111     C*
022000070112     c                   delete    fiFTT02
022100070112     C*
022200070112     c     salta         tag
022300070112     C*
022400070112     C     kFTT          reade     Fiftt02L
022500070112     c                   enddo
022600070112     C*
022700070112     C                   EndSR
022800070112     C*---------------------------------------------------------------*
022900070112     C     write_FCE     BegSR
023000070112     C*
023100070112     c                   if        ftdtsr = 'C'
023200070112     c                   move      '005'         w01CCE
023300070112     c                   end
023400070112     C*
023500070112     c                   if        ftdtsr = 'R'
023600070112     c                   move      '001'         w01CCE
023700070112     c                   end
023800070112     C*
023900070112     C*  se il codice Autista Competenze da attribuire è impostato o no
024000070112     c                   if        vidAU2 > 0
024100070112     c                   eval      w01PDR = vidAU2
024200070112     c                   else
024300070112     c                   eval      w01PDR = FtdPDR
024400070112     c                   end
024500070112     C*
024600070112     C     kFCE          chain     FiFCE01l
024700070112     c                   if        not %Found(FiFCE01l)
024800070112     C                   clear                   fiFCE000
024900070112     c                   eval      FCEPDR =  w01PDR
025000070112     c                   eval      FCETSR =  FtdTSR
025100070112     c                   eval      FCEFGS =  FtdFGS
025200070112     c                   eval      FCENDC =  FtdNDC
025300070112     c                   eval      FCEDDC =  FtdDDC
025400070112     c                   eval      FCEAAS =  FtdAAS
025500070112     c                   eval      FCELNP =  FtdLNP
025600070112     c                   eval      FCENRS =  FtdNRS
025700070112     c                   eval      FCENSP =  FtdNSP
025800070112     c                   eval      FCECCE =  w01CCE
025900070112
026000070112     C                   write     fiFCE000
026100070112     C                   End
026200070112     C*
026300070112     C                   EndSR
026400070112     C*****************************************************************
026500070112     C     *inzsr        BegSR
026600070112      *
026700070112     C     *ENTRY        PLIST
026800070112     C                   PARM                    KPJBA
026900070112      *
027000091103     c                   seton                                        33
027100091103     c                   if        %subst(knmus:1:3) = 'EDP'
027200091103     c                   setoff                                       33
027300091103     c                   end
027400091103      *
027500070112     C                   Z-ADD     1             CODUT
027600070112     C                   CALL      'X§PARUT'
027700070112     C                   PARM                    UT§DSE
027800070112     C                   MOVEL     RAGUT         vidrsu
027900070112     C                   MOVEL     REC80         CNCR80
028000091103     C***
028100091103     C* CARICO TABELLA FILIALI GESTITE £1
028200091103     C***
028300091103     C                   CLEAR                   DSUL06
028400091103     C                   MOVE      '£1'          D06COD
028500091103     C                   MOVEL     SIMFEL        D06KEY
028600091103     C                   MOVEL     ' '           D06TLA
028700091103     C                   MOVEL     DSUL06        KPJBU
028800091103     C                   CALL      'TRUL06R'
028900091103     C                   PARM                    KPJBA
029000091103     C                   MOVEL     KPJBU         DSUL06
029100091103     C                   MOVEA     LIN           L1
029200091103     C***
029300070112     C*---------------------------------------------------------------*
029400070112     C     kFTT          KLIST
029500070112     C                   KFLD                    VIDAU1
029600070112     C                   KFLD                    W01FVL
029700070112     C                   KFLD                    W01DDC
029800070112      *
029900070112     C     kFTT01        KLIST
030000070112     C                   KFLD                    VIDAU1
030100070112     C                   KFLD                    W01TSR
030200070112     C                   KFLD                    VIDNDC
030300070112     C                   KFLD                    W01DDC
030400070112      *
030500070112     C     kFTD          KLIST
030600070112     C                   KFLD                    FTTPDR
030700070112     C                   KFLD                    FTTTSR
030800070112     C                   KFLD                    FTTNDC
030900070112     C                   KFLD                    FTTDDC
031000070112     C*
031100070112     C     kFCE          KLIST
031200070112     C                   KFLD                    FtdFGS
031300070112     C                   KFLD                    w01PDR
031400070112     C                   KFLD                    FtdTSR
031500070112     C                   KFLD                    FtdNDC
031600070112     C                   KFLD                    FtdDDC
031700070112     C                   KFLD                    FtdAAS
031800070112     C                   KFLD                    FtdLNP
031900070112     C                   KFLD                    FtdNRS
032000070112     C                   KFLD                    FtdNSP
032100070112     C                   KFLD                    w01CCE
032200070112     C*
032300070112     C     kFRE          KLIST
032400070112     C                   KFLD                    Ktip              1
032500070112     C                   KFLD                    VIDAU1
032600070112     C                   KFLD                    W01DDC
032700070112     C                   KFLD                    W01TSR
032800070112     C*
032900070112     C     kFRE1         KLIST
033000070112     C                   KFLD                    Ktip              1
033100070112     C                   KFLD                    VIDAU1
033200070112     C                   KFLD                    W01DDC
033300070112     C*
033400070112     C     kapd          KLIST
033500070112     C                   KFLD                    Ktip              1
033600070112     C                   KFLD                    VIDAU1
033700070112     C*
033800070112     C     kapd1         KLIST
033900070112     C                   KFLD                    Ktip              1
034000070112     C                   KFLD                    VIDAU2
034100070112      *
034200070112     C                   move      *blank        W01FVL
034300070112     C                   move      'A'           Ktip              1
034400070112     C*
034500070112     C* DEFINIZIONE CAMPI
034600070112     C     *LIKE         DEFINE    FTTDDC        W01DDC
034700070112     C     *LIKE         DEFINE    FTTTSR        W01TSR
034800070112     C     *LIKE         DEFINE    FTTFVL        W01FVL
034900070112     C     *LIKE         DEFINE    FCECCE        W01CCE
035000070112     C     *LIKE         DEFINE    FCEPDR        W01PDR
035100070112     C*
035200070112     C* LA DATA DEL GIORNO LA PRENDO DA TIME
035300070112     C                   TIME                    W0140            14 0
035400070112     C                   MOVE      W0140         UDATE8            8 0
035500070112     C*
035600070112     C                   EndSR
035700070112     C*---------------------------------------------------------------*
