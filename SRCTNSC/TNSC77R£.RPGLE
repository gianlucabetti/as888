000100030617     H DECEDIT('0,')
001700000000     F*----------------------------------------------------------------H
001800030324     F*------- TNSC77R -  CREA WORK X STAMPE C/A DIRETTI          -----
001900000000     F*----------------------------------------------------------------
001901030324     FFNSPE03L  IF   E           K DISK
001902030324     FTNCSA01L  IF   E           K DISK
001903030324     FAZORG01L  IF   E           K DISK
002000941031     FTNCSB02L  IF   E           K DISK
002100030325     FTNCSBM1K  O    E             DISK
002101030326     Fqsysprt   O    f  132        printer oflind(*inof)
002300971216     D D4Q             S             89    DIM(100)
002400971216     D K4Q             S              3    DIM(100)
002401030324     D fil             S              3  0 DIM(999)
002402030324     D rag             S              3  0 DIM(999)
002403030324     D* ORDINAM. X LETTURA TNCSB
002500971216     D*----------------------------------------------------*
002700971216     D KPJBA         E DS
002701030617     D FNLV55        E DS                  EXTNAME(FNLV55DS)
002702030324     D* DS PER TISI95R - CONTROLLO CAP
002703030324     D DSSI95        E DS                  EXTNAME(TISI95DS)
002704030324     D TIBS69        E DS                  EXTNAME(TIBS69DS)
002705030324     D CNACO         E DS                  EXTNAME(CNACO00F)
002706030324     D CNIND         E DS                  EXTNAME(CNIND00F)
002707030324     D CNCLP         E DS                  EXTNAME(CNCLP00F)
002708030324     D FNCLS         E DS                  EXTNAME(FNCLS00F)
002709030617     d*
002710030617     d dtaiso          s               d   inz
004800030617     d*----------------------------------------------------*
005000930319     C* MI POSIZIONO CON TPI=M + BNA=0
009100030324     C     KCSB          SETLL     TNCSB02L
009101030324     C                   DO        *HIVAL
009102030324     C     KCSB          READE     TNCSB02L
009200030324     C                   IF        %EOF(TNCSB02L)
009201030324     C                   LEAVE
009202030324     C                   END
009300930319     C* NON CONFERMATO INCASSO
009400910619     C     CSBDTA        IFLE      *ZEROS
009500030324     C                   ITER
009600900307     C                   END
009601030326     C* ometto annullati
009602030326     C     CSBsta        IFeq      9
009603030326     C                   ITER
009604030326     C                   END
010200971216      * SE CLIENTE NON CODIFICATO 9999 FORZO LNP
010300930319     C                   MOVE      CSBCDI        §QUATR            4 0
010400930319     C     §QUATR        IFEQ      *ALL'9'
010500930319     C                   MOVEL     CSBLNP        CSBCDI
010600930319     C                   END
010601030324     C* RICERCO IL TERMINAL DI ARRIVO E LA ZONA PER BARCODE
010602030324     C                   EXSR      SRBAR
011101030326     c* se non trova anagrafico o il reperimento del cappario non ha esito
011102030326     c* positivo non scrivo il record sul file FIVABCAF e segnalo errore
011103030326     c                   if        o95err =  *blanks
011200030324     C                   WRITE     TNCSBW00
011201030326     c                   else
011202030326     c                   exsr      stampa
011203030326     c                   end
011300030324     C                   ENDDO
011302030617     C* CALL A VUOTO AL PGM FNLV55R PER CHIUDERE I FILES
011303030617     C                   CLEAR                   FNLV55
011304030617     C                   MOVEL     'L'           D55TLA
011305030617     C                   CALL      'FNLV55R'
011306030617     C                   PARM                    FNLV55
011307030324     C* CALL A VUOTO AL PGM TISI95R PER CHIUDERE I FILES
011308030324     C                   CLEAR                   DSSI95
011309030324     C                   MOVEL     'C'           I95TLA
011310030324     C                   CALL      'TISI95R'
011311030326     C                   PARM                    DSSI95
011312030324     C*
011401030324     C                   SETON                                        LR
011402030324     C*---------------------------------------------------------------*
011403030324     C     SRBAR         BEGSR
011404030324     C*---------------------------------------------------------------*
011406030324     C                   CLEAR                   DSSI95
011407030325     C* NEL FILE ESISTONO DUE RIFERIMENTI AL DESTINATARIO: 666 E INDIRIZZO
011408030325     C* ABITUALE
011409030324     c* no clienti 8888 o 9999
011410030324     C     §QUATR        IFne      8888
011411030324     C     §QUATR        andne     9999
011412030325     C* SE TROVO PRENDO DA FNSPE INDIRIZZO DELLA SPEDIZIONE
011413030324     C     KSPE          CHAIN     FNSPE03l
011414030324     C                   if        %found(fnspe03l)
011415030324     C                   MOVEL     SPERAG        csbrsm
011416030325     C                   MOVEL     SPEIND        csbinm
011417030324     C                   MOVEL     SPELOC        i95loc
011418030324     C                   MOVEL     SPECAP        i95cap
011419030324     C                   MOVEL     SPEPRO        i95prv
011420030324     C                   MOVEL     SPENAZ        i95nar
011421030327     C* RICHIAMO IL PGM TISI95R PER REPERIRE LA ZONA
011422030327     C                   MOVEL     '3'           I95TCN
011423070119     C                   MOVE      *date         dtaiso
011424070119     C                   MOVE      dtaiso        i95dat
011425070119     C*                  MOVEL     csbaas        I95DAT
011426070119     C*                  MOVE      csbmgs        I95DAT
011427030327     C                   CALL      'TISI95R'
011428030327     C                   PARM                    DSSI95
011429030327     C     O95ERR        IFeq      *BLANKS
011430030327     c                   eval      csbzsc = o95znc
011431030327     C                   eval      csbpoa = o95lna
011432030327     C                   eval      csblom = o95loc
011433030327     C                   eval      csbcam = o95cap
011434030327     C                   eval      csbprm = o95prv
011435030327     C                   eval      csbnzm = o95nar
011441030327     c                   end
011442030327     C                   end
011443030325     C* PRENDO DA CNACO l'indirizzo abituale
011444030324     C                   CLEAR                   TIBS69
011445030324     C                   CLEAR                   CNACO
011446030324     C                   CLEAR                   CNIND
011447030324     C                   CLEAR                   CNCLP
011448030324     C                   CLEAR                   FNCLS
011449030324     C                   MOVEL     ' '           I69TLA
011450030324     C                   MOVEL     KNSIF         I69SIF
011451030324     C                   Z-ADD     0             I69KCC
011452030326     C                   Z-ADD     csbcdi        I69KAC
011453030326     C                   Z-ADD     csbcdi        I69KIN
011454030326     C                   Z-ADD     csbcdi        I69KCP
011455030324     C                   CALL      'TIBS69R'
011456030324     C                   PARM                    TIBS69
011457030324     C                   PARM                    CNACO
011458030324     C                   PARM                    CNIND
011459030324     C                   PARM                    CNCLP
011460030324     C                   PARM                    FNCLS
011461030324     C     O69ERR        IFEQ      *BLANK
011462030327     C* SE NON ESISTE 666 prendo i dati anagrafici del destinatario
011463030327     C* della spedizione dall'anagrafico
011464030327     c                   if        i95loc = ' '
011465030620     C                   movel(P)  ACORAG        csbrsm
011466030620     C                   MOVEL(p)  INDVIA        csbinm
011467030620     C                   MOVEL(p)  INDCIT        csblom
011468030324     C     INDCAE        IFNE      *BLANK
011469030620     C                   MOVEL(p)  INDCAE        csbcam
011470030324     C                   ELSE
011471030620     C                   MOVEL(p)  INDCAP        csbcam
011472030324     C                   END
011473030620     C                   MOVEL(p)  INDPRV        csbprm
011474030620     C                   MOVEL(p)  INDSTA        csbnzm
011475030327     c                   eval      csbzsc = 0
011476030327     C                   movel     csbcdi        csbpoa
011477030327     c                   end
011481030325     C* RIEMPO COMUNQUE I DATI DELl'indirizzo ABITUALE
011482030620     C                   MOVEL(p)  ACORAG        csbrsc
011483030620     C                   MOVEL(p)  INDVIA        csbinD
011484030620     C                   MOVEL(p)  INDCIT        csbloD
011485030325     C     INDCAE        IFNE      *BLANK
011486030620     C                   MOVEL(p)  INDCAE        csbcaD
011487030325     C                   ELSE
011488030620     C                   MOVEL(p)  INDCAP        csbcaD
011489030325     C                   END
011490030620     C                   MOVEL(p)  INDPRV        csbprD
011491030620     C                   MOVEL(p)  INDSTA        csbnZD
011492030327     c                   else
011493030327     c                   eval      o95err = o69err
011494030324     C                   END
011496030324     c* per 8888 o 9999 leggo tncsa01l
011497030324     C                   ELSE
011498030324     C     Knsp          CHAIN     TNCSA01l
011499030324     c                   if        %found(tncsa01l)
011500030327     c                   eval      csbzsc = 0
011501030327     C                   movel     csbcdi        csbpoa
011502030324     C                   MOVEL     CSARSD        csbrsm
011503030324     C                   MOVEL     CSAVID        csbinm
011504030326     C                   MOVEL     CSARSD        csbrsc
011505030325     C                   MOVEL     CSAVID        csbinD
011506030325     C                   MOVEL     CSACID        csblod
011507030325     C                   MOVEL     CSACAD        csbcad
011508030325     C                   MOVEL     CSAPRM        csbprd
011509030325     C                   MOVEL     CSANZM        csbnzd
011510030327     C                   MOVEL     CSACID        csblom
011511030327     C                   MOVEL     CSACAD        csbcam
011512030327     C                   MOVEL     CSAPRM        csbprm
011513030327     C                   MOVEL     CSANZM        csbnzm
011514030327     c                   else
011515030327     c                   eval      o95err = '1'
011516030324     C                   END
011517030324     C                   END
011518030326     c* se non trova anagrafico non scrivo il record sul file FIVABCAF
011519030327     c                   if        o95err = *blanks
011520030327     c                   z-add     1             a
011521030327     c     csbpoa        lookup    fil(a)                                 96
011522030327     c                   if        *in96
011523030327     c                   eval      csbtfa = rag(a)
011524030327     c                   else
011525030327     c                   eval      o95err = '1'
011526030327     c                   end
011527030326     c                   end
011528030324     C*
011529030324     C                   ENDSR
014100030324     C*---------------------------------------------------------------*
014400030326     C     stampa        BEGSR
014401030324     C*---------------------------------------------------------------*
014402030326     c*
014403030326     c   of              except    testa
014404030326     c                   setoff                                       of
014405030326     c                   except    errore
014501030326     C*
014502030326     C                   ENDSR
014503030326     C*---------------------------------------------------------------*
014504030326     C     *INZSR        BEGSR
014505030326     C*---------------------------------------------------------------*
014506030326     C     *ENTRY        PLIST
014600030324     C                   PARM                    KPJBA
014800030324     C* DEFINIZIONE CHIAVI
015000030324     C     KCSB          KLIST
015100030324     C                   KFLD                    §TPI
015200030324     C                   KFLD                    §BNA
015201030324     C     KSPE          KLIST
015202030324     C                   KFLD                    spefls
015203030324     C                   KFLD                    csbcdi
015204030324     C                   KFLD                    COD               3
015205030324     C                   MOVE      '666'         COD
015206030324     C                   MOVEL     'L'           spefls
015207030324     C*
015208030324     C     KNSP          KLIST
015209030324     C                   KFLD                    CSBAAS
015210030324     C                   KFLD                    CSBLNP
015211030324     C                   KFLD                    CSBNRS
015212030324     C                   KFLD                    CSBNSP
015300030324     C*
015400030324     C                   MOVEL     'M'           §TPI              1
015500030324     C                   MOVE      *ZEROS        §BNA
015501030326     c                   seton                                        of
015600030324     C*
015700030324     C     *LIKE         DEFINE    CSBBNA        §BNA
015800030324     C*
015801030324     c                   exsr      carcdc
015900030324     C                   ENDSR
016000030324     C**-------------------------------------------------------
016100030324     C*+ CARICAMENTO RAGGRUPPAMENTI FILIALE 1°LIVELLO CONTABIL.
016200030324     C**-------------------------------------------------------
016300030324     C     CARCDC        BEGSR
016400030324     C                   SETOFF                                       96
016500030324     C                   clear                   fil
016501030324     C                   clear                   rag
016600030324     C     *LOVAL        SETLL     AZORG
016700030324     C                   DO        999           A                 4 0
016800030324     C                   READ      AZORG                                  96
016900030324     C  N96ORGFAG        IFEQ      'F'
017000030324     C     ORGFAG        OREQ      'A'
017100030324     C                   MOVE      ORGFIL        FIL(a)
017101030617      *  Reperisco terminal di arrivo
017102030617     C                   CLEAR                   FNLV55
017103030617     C* Imposto i dati in LV55
017106030617     C                   MOVE      *date         dtaiso
017107030617     C                   MOVE      dtaiso        D55DRF
017108030617     C                   MOVEL     ORGFIL        D55LIN
017109030618     c* forzo Bologna come terminal x controllare le eccezioni xchè 102 non
017110030618     c* è detto che esista
017111030618     C                   MOVE      001           D55Lnp
017112030617     C                   CALL      'FNLV55R'
017113030617     C                   PARM                    FNLV55
017114030617     C                   MOVE      D55TFA        RAG(a)
018100030324     C                   END
018200030324     C  N96              END
018300030324     C*
018500030324     C                   ENDSR
018600030326     oqsysprt   e            testa
018700030326     o                       udate               +0 '  /  /  '
018800030326     o                                           70 'ERRORE REPERIMETO ANAGRAFI-
018900030326     o                                              CO CLIENTI'
019000030326     o                                           +5 'TNSC77R'
019100030326     o          e            errore
019200030326     o                                           +0 'Anno'
019300030326     o                       csbaas              +1
019400030326     o                                           +1 'lnp'
019500030326     o                       csblnp              +1
019600030326     o                                           +1 'serie'
019700030326     o                       csbnrs        2     +1
019800030326     o                                           +1 'numero'
019900030326     o                       csbnsp        2     +1
020000030326     o                                           +1 'cliente'
020100030326     o                       csbcdi        2     +1
