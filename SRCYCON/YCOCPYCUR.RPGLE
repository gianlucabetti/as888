000100981202     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND) ACTGRP(QILE)
000200970620     H*PARMS CVTOPT(*NONE)
000300150219     H DECEDIT('0,') DATEDIT(*DMY.)
000301150219     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('PJXBND')
000400970620      *
000500080404      *       GESTIONE CATALOGO FILE upload su IFS
000600080404      *  -----------------------------------------------------
000700970620      *
000800150219      *
001600150219     FANCFM01L  UF   E           K DISK
001601161010     Fancft01l  UF   E           K disk
001700970620     D*-------------
001800150219     D* Passaggio Parametri
006300970620     D KPJBA         E DS
006400150219     D Ana123DS      E DS
006500970620     D*-------------
006600970620     D* DS Esterna gestione messaggi
006700080421     D XMSGDS        E DS
006800080421     D  STS          E                     EXTFLD(MSGSTS)
006900080421     D                                     DIM(333)
007000080421     D  JBA          E                     EXTFLD(MSGJBA)
007100080421     D                                     DIM(502)
007200080423     D  MSGEMV       E                     INZ('S')
007300080423     D  MSGMSG       E                     INZ(0)
007400080423     D  MSGRGP       E                     INZ(10)
007500080423     D  MSGCNL       E                     INZ('N')
007600080423     D  MSGVID       E                     INZ('N')
007700080423     D  MSGOPE       E                     INZ('N')
007800080423     D  MSGSTP       E                     INZ('N')
007900080423     D  MSGlck       E                     INZ('N')
008000080421     D*-------------
008100080421     D* Reperimento nome PGM
008200080421     D STATUS         SDS           333
008300080421     D  DSPGM            *PROC
008400080421     D  PARMS            *PARMS
008500080421     D*-------------
008600080421     D* Dati di ambiente ottenuti da XSOC
008700080421     D SOC001        E DS                  EXTNAME(XSOC001DS)
008800080421     D*-------------
008900080421     D* DS Interna per dati di output di XSOC
009000080421     D XSOCDS          DS          1000
009100080421     D*-------------
009200150219     D* Reperimento Autorizzazioni
009800080421     D AUTDS         E DS                  EXTNAME(XCHKAUTDS)
009900080421     D* Maschera dati autorizzazioni
010000080423     D ANGCFIDS      E DS
010100080421     D*-------------
010200080404     D ZDateISO        S               D
010300150219      *----------------------------------------------------*
012600080421      *                MAIN LINE PROGRAM
012700080421      *----------------------------------------------------*
012800080421      *
012900080422      * Inizializzazione variabili
013000970620     C                   EXSR      INZVAR
013100150219      *
013102150219      * Controllo autorizzazioni
013103150219     C                   EXSR      CtrAUT
013104150219      * Alloco ancfm00f
013300080404     C                   EXSR      ReadCfm
013301161010      * Leggo ancft00f
013302161010     C                   EXSR      ReadCft
013303161010      * exec.
015500080422     C                   EXSR      ExeSca
015600080422      *
015700150219      * Se ok, aggiorno ANCFM00F
015800080422      * flaggo il file come scaricato
015900080422     C                   Move      *On           CFMScar
016000080422     C                   Update    ANCFM000
016100080422     C                   Move      *On           Opr123
016400080421      *
016500080421      * fine programma
016600970620     C                   EXSR      ENDPGM
016700150219     C************************************************************
044600080421     C* controllo autorizzazioni
044700080421     C************************************************************
044800080421     C     CtrAUT        BegSR
044900080421      *
045000080422     C                   EXSR      REPAUT
045100080421     C                   If        UPLSSM  <> *on
045200080422     C                   MOVE      '2'           Err123
045300080422     C* Utente/profilo non abilitato alla funzione
045400080422     C                   Eval      IDMSG = 'PROMSG    *LIBL     COS0011'
045500080422     C                   EXSR      ENDPGM
045600080421     C                   Endif
045700080421      *
045800080421     C                   ENDSR
045900080421     C/EJECT
046000080421     C************************************************************
046100080421     C* REPERIMENTO AUTORIZZAZIONI UTENTE
046200080421     C************************************************************
046300080421     C     REPAUT        BEGSR
046400080421     C*
046500080421     C                   CLEAR                   AUTDS
046600080421     C                   MOVEL     CfmSocieta    XCASOC                         SOCIETA
046700080421     C                   MOVEL     XSCDUN        XCAUNI                         UNITA
046800080421     C                   Z-ADD     0             XCAGRP                         GRUPPO
046900080421     C                   MOVEL     KNMUS         XCAPRF                         PROFILO
047000080422     C                   MOVEL     'ANGCFI'      XCAFNC                         FUNZIONE
047100080422     C                   MOVEL     CfmTpScar     XCAVFU                         VARIABILE
047200080422     C                   MOVE      'CC'          XCATCK                         TIPO CTRL
047300080422     C                   MOVE      '1'           XCANAU                         RICH.XNOAUT
047400080421     C*
047500150219     C                   CALLB     'XCHKAUT'
047600080421     C                   PARM                    AUTDS
047700080421     C*
047800080421     C* errori/non abilitazione
047900080421     C     XCARTN        IFNE      0
048000080422     C                   MOVE      '2'           Err123
048100080422     C* Utente/profilo non abilitato alla funzione
048200080422     C                   Eval      IDMSG = 'PROMSG    *LIBL     COS0011'
048300080422     C                   EXSR      ENDPGM
048400080421     C                   ELSE
048500080422     C                   MOVEL     XCADDS        AngCFIDs
048600080421     C                   ENDIF
048700080421     C*
048800080421     C                   ENDSR
048900150219     C************************************************************
051900080429     C* ESEGUE upload
052000080421     C************************************************************
052100080421     C     EXESCA        BEGSR
052200080421      *
052300150219     C                   call      'YCOCPYCU'
052600150219     C                   Parm                    soc123
052700150219     C                   Parm                    File123
053000161010     C                   Parm      *off          ErrCU             1
053001161010     C                   Parm      cftPercIFS    ifsdir          255
053300080421      *
053400080421      * Se fallito lo scarico / non trovato il percorso
053500150219     C                   If        ErrCU = *on
053600080422     C* Fallito upload su IFS
053700080429     C                   Eval      IDMSG = 'PROMSG    *LIBL     PRO2327'
053800080421     C                   Move      '1'           Err123
053900080422     C                   EXSR      ENDPGM
054000080421     C                   EndIf
054100080404      *
054200970623     C                   ENDSR
054400970623     C************************************************************
054500080404     C* Read ancfm allocando
054600970623     C************************************************************
054700080404     C     ReadCFM       BEGSR
054800080421      *
054900080404     C                   MOVEL     Soc123        CFMSocieta
055000080404     C                   MOVEL     File123       CFMFile
055100150219      *
055400080404     C     *IN22         DOUEQ     *OFF
055500970623     C     MSGRTN        ORNE      'R'
055600080421      * il rcd che leggo viene bloccato sempre
055700150219     C     k02cfm01      CHAIN     ANCFM000                           2122
055800080421      *
055900080421      * Gestione RCD allocato
056000080404     C     *IN22         IFEQ      *ON
056100080404     C                   MOVEL     'ANCFM00F'    MSGFIL
056200970623     C                   EXSR      MSGPGM
056300080404     C                   ENDIF
056400080404     C                   ENDDO
056500080421      *
056600080421      * se il rcd da gestire non viene trovato
056700080421      * oppure è allocato da altro pgm
056800080421      * ritorno al pgm guida avvertendolo dell'errore
056900080404     C     *IN21         IFEQ      *ON
057000970623     C     *IN22         OREQ      *ON
057100080404     C                   MOVE      '1'           ERR123
057200080422     C* errore generico
057300080422     C                   Eval      IDMSG = 'PROMSG    *LIBL     PRO0007'
057400080422     C                   EXSR      ENDPGM
057500080404     C                   ENDIF
057600080421      *
057700970623     C                   ENDSR
057701161010     C************************************************************
057702161010     C* Read ancfm allocando
057703161010     C************************************************************
057704161010     C     ReadCFT       BEGSR
057705161010      *
057706170217     C                   if        cfmtpscar <> 'CU'
057707161010     C                   MOVEL     cfmsocieta    CFTSocieta
057708170217     C                   else
057709170217     C                   clear                   CFTSocieta
057710170217     C                   endif
057711161010     C                   MOVEL     cfmtpscar     CFTTRACC
057712161010      *
057713161010     C     *IN22         DOUEQ     *OFF
057714161010     C     MSGRTN        ORNE      'R'
057715161010      * il rcd che leggo viene bloccato sempre
057716161010     C     k02cft01      CHAIN     ANCFT000                           2122
057717161010      *
057718161010      * Gestione RCD allocato
057719161010     C     *IN22         IFEQ      *ON
057720161010     C                   MOVEL     'ANCFT00F'    MSGFIL
057721161010     C                   EXSR      MSGPGM
057722161010     C                   ENDIF
057723161010     C                   ENDDO
057724161010      *
057725161010      * se il rcd da gestire non viene trovato
057726161010      * oppure è allocato da altro pgm
057727161010      * ritorno al pgm guida avvertendolo dell'errore
057728161010     C     *IN21         IFEQ      *ON
057729161010     C     *IN22         OREQ      *ON
057730161010     C                   MOVE      '1'           ERR123
057731161010     C* errore generico
057732161010     C                   Eval      IDMSG = 'PROMSG    *LIBL     PRO0007'
057733161010     C                   EXSR      ENDPGM
057734161010     C                   ENDIF
057735161010      *
057736161010     C                   ENDSR
057737161010     C************************************************************
057738150219     C* Fine Programma
057739150219     C************************************************************
057740150219     C     ENDPGM        BEGSR
057741150219     C*
057742150219     C                   UNLOCK    ANCFM01L                             22
057743150219     C*
057744150219     C                   If        Err123   <> *off
057745150219     C                   Exsr      RepTxt
057746150219     C                   MOVEL     WTxt          TxtErr123
057747150219     C                   Endif
057748150219     C*
057749150219     C                   Eval      KPJBU  =  ANA123DS
057750150219     C                   SETON                                            LR
057751150219     C                   RETURN
057752150219     C*
057753150219     C                   ENDSR
057754150219     C************************************************************
057755150219     C* Reptxt
057756150219     C************************************************************
057757150219     C     Reptxt        BEGSR
057758150219      *
057759150219     C                   Clear                   WTxt            135
057760150219     C                   CALLB     'XRTVM'
057761150219     C                   PARM                    IDMSG            27
057762150219     C                   PARM                    TXTMSG          100
057763150219     C                   PARM                    ERRMSG            1
057764150219     C                   PARM      *OFF          CTRMSG            1            centratura
057765150219     C                   PARM      135           LENMSG            3 0          lun output.
057766150219     C     ERRMSG        IFNE      *ON
057767150219     C                   MOVEL     TXTMSG        Wtxt
057768150219     C                   Endif
057769150219      *
057770150219     C                   ENDSR
057771150219     C******************************************************
057900080404     C* PGM PER GESTIONE MESSAGGI ERRORE                   *
058000080404     C******************************************************
058100080404     C     MSGPGM        BEGSR
058200080421      *
058300080404     C                   Z-ADD     2             MSGMSG
058400080404     C                   MOVEL     'S'           MSGRSP
058500080404     C                   MOVEL     'CR'          MSGTPR
058600080404     C                   MOVEL     'S'           MSGEMV
058700080404     C                   Z-ADD     10            MSGRGP
058800080404     C                   MOVEL     'S'           MSGLCK
058900080404     C                   MOVEA     STATUS        STS
059000080404     C                   MOVEL     'N'           MSGCNL
059100080404     C                   MOVEL     'N'           MSGVID
059200080404     C                   MOVEL     'N'           MSGOPE
059300080404     C                   MOVEL     'N'           MSGSTP
059400080404     C                   CALL      'XMSGR'
059500080404     C                   PARM                    XMSGDS
059600080421      *
059700080404     C                   ENDSR
059800150219     C************************************************************
059801150219     C* OPERAZIONI INIZIALI
059802150219     C************************************************************
059803150219     C     *pssr         BEGSR
059804150219      *
059805150219     C* errore generico
059807150219     C                   MOVE      '1'           ERR123
059808150219     C                   Eval      KPJBU  =  ANA123DS
059809150219     C                   SETON                                            LR
059810150219     C                   return
059811150219      *
059813150219     C                   ENDSR
059814150219     C************************************************************
060000970620     C* OPERAZIONI INIZIALI
060100970620     C************************************************************
060200970620     C     *INZSR        BEGSR
060300080522      *
060400080404     C     *DMY          Move      Udate         ZDateISO
060500080404     C     *JobRun       Move      ZDateISO      ZDate             6 0
060600080421      *
060700080421      * Reperimento parametri
060800970620     C     *ENTRY        PLIST
060900970620     C                   PARM                    KPJBA
061000150219      *
061400970620     C                   ENDSR
061500150219     C************************************************************
061700970620     C* INIZIALIZZAZIONE VARIABILI
061800970620     C************************************************************
061900970620     C     INZVAR        BEGSR
062000080421      *
062100080421      * Dati da PGM guida
062200150219     C                   MOVEL     KPJBU         ANA123DS
062300150219     C                   Clear                   TxtErr123
062400080422     C                   Eval      Err123 = *off
062500080404     C                   Eval      Opr123 = *off
062600080421      *
062700970620     C                   MOVEL     'SOC001'      TIPXSC
062800080404     C                   MOVEL     Societa123    SOCXSC
062900970620     C                   EXSR      REPSOC
063000970620     C     RTNXSC        IFNE      '1'
063100970620     C                   MOVEL     XSOCDS        SOC001
063200970620     C                   ELSE
063300080404     C                   MOVE      '1'           Err123
063400080422     C* Errore generico
063500080422     C                   Eval      IDMSG = 'PROMSG    *LIBL     PRO0007'
063600970620     C                   EXSR      EndPgm
063700970620     C                   ENDIF
063702150219      *
063900080404     C                   ENDSR
064000150219     C************************************************************
064200970620     C* REPERIMENTO DATI SOCIETA'
064300970620     C************************************************************
064400970620     C     REPSOC        BEGSR
064500080421      *
064600970620     C                   CALLB     'XSOC'
064700970620     C                   PARM                    TIPXSC            6
064800970620     C                   PARM                    SOCXSC            3
064900970620     C                   PARM                    CDSXSC            9 0
065000970620     C                   PARM                    MODXSC            3
065100970620     C                   PARM      *BLANKS       RTNXSC            1
065200970620     C                   PARM                    XSOCDS
065300970620     C                   PARM                    KPJBA
065400080421      *
065500970620     C                   ENDSR
065600150219     C************************************************************
087000970620     C* DEFINIZIONE KLIST
087100970620     C************************************************************
087200970620     C     DefKlist      BEGSR
087300080421      *
087400080421      * klist
087500080421      *
087600161010      * ANCFM01L
087601161010     C     k02cfm01      KLIST
087602161010     C                   KFLD                    CFMSocieta
087603161010     C                   KFLD                    CFMFile
087604161010      * ANCFT01L
087605161010     C     k02cft01      KLIST
087606161010     C                   KFLD                    CFTSocieta
087607161010     C                   KFLD                    CFTTracc
087608161010      *
088300970620     C                   ENDSR
