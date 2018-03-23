000100030617     H DECEDIT('0,')
000200000000     F*----------------------------------------------------------------H
000300030324     F*------- TNSC77R -  CREA WORK X STAMPE C/A DIRETTI          -----
000400000000     F*----------------------------------------------------------------
000500030324     FFNSPE03L  IF   E           K DISK
000600030324     FTNCSA01L  IF   E           K DISK
000700030324     FAZORG01L  IF   E           K DISK
000800941031     FTNCSB02L  IF   E           K DISK
000900120222     FTNCSm03L  IF   E           K DISK
001000030325     FTNCSBM1K  O    E             DISK
001100030326     Fqsysprt   O    f  132        printer oflind(*inof)
001200971216     D D4Q             S             89    DIM(100)
001300971216     D K4Q             S              3    DIM(100)
001400030324     D fil             S              3  0 DIM(999)
001500030324     D rag             S              3  0 DIM(999)
001600030324     D* ORDINAM. X LETTURA TNCSB
001700971216     D*----------------------------------------------------*
001800971216     D KPJBA         E DS
001900030617     D FNLV55        E DS                  EXTNAME(FNLV55DS)
002000030324     D* DS PER TISI95R - CONTROLLO CAP
002100030324     D DSSI95        E DS                  EXTNAME(TISI95DS)
002200030324     D TIBS69        E DS                  EXTNAME(TIBS69DS)
002300030324     D CNACO         E DS                  EXTNAME(CNACO00F)
002400030324     D CNIND         E DS                  EXTNAME(CNIND00F)
002500030324     D CNCLP         E DS                  EXTNAME(CNCLP00F)
002600030324     D FNCLS         E DS                  EXTNAME(FNCLS00F)
002700030617     d*
002800030617     d dtaiso          s               d   inz
002900030617     d*----------------------------------------------------*
003000930319     C* MI POSIZIONO CON TPI=M + BNA=0
003100030324     C     KCSB          SETLL     TNCSB02L
003200030324     C                   DO        *HIVAL
003300030324     C     KCSB          READE     TNCSB02L
003400030324     C                   IF        %EOF(TNCSB02L)
003500030324     C                   LEAVE
003600030324     C                   END
003700930319     C* NON CONFERMATO INCASSO
003800910619     C     CSBDTA        IFLE      *ZEROS
003900030324     C                   ITER
004000900307     C                   END
004100030326     C* ometto annullati
004200030326     C     CSBsta        IFeq      9
004300030326     C                   ITER
004400030326     C                   END
004500971216      * SE CLIENTE NON CODIFICATO 9999 FORZO LNP
004600930319     C                   MOVE      CSBCDI        §QUATR            4 0
004700930319     C     §QUATR        IFEQ      *ALL'9'
004800930319     C                   MOVEL     CSBLNP        CSBCDI
004900930319     C                   END
005000030324     C* RICERCO IL TERMINAL DI ARRIVO E LA ZONA PER BARCODE
005100030324     C                   EXSR      SRBAR
005200030326     c* se non trova anagrafico o il reperimento del cappario non ha esito
005300030326     c* positivo non scrivo il record sul file FIVABCAF e segnalo errore
005400030326     c                   if        o95err =  *blanks
005500120222     c                   if        %subst(csbnra: 10: 1) = ' '
005600120222     c                   clear                   csbprg
005700120222     c                   eval      csbidc = csbcas
005800030324     C                   WRITE     TNCSBW00
005900120222     c                   else
006000120222     c                   exsr      srass
006100120222     c                   end
006200030326     c                   else
006300030326     c                   exsr      stampa
006400030326     c                   end
006500030324     C                   ENDDO
006600030617     C* CALL A VUOTO AL PGM FNLV55R PER CHIUDERE I FILES
006700030617     C                   CLEAR                   FNLV55
006800030617     C                   MOVEL     'L'           D55TLA
006900030617     C                   CALL      'FNLV55R'
007000030617     C                   PARM                    FNLV55
007100030324     C* CALL A VUOTO AL PGM TISI95R PER CHIUDERE I FILES
007200030324     C                   CLEAR                   DSSI95
007300030324     C                   MOVEL     'C'           I95TLA
007400030324     C                   CALL      'TISI95R'
007500030326     C                   PARM                    DSSI95
007600030324     C*
007700030324     C                   SETON                                        LR
007800120222      *---------------------------------------------------------------*
007900120222      *  stampa rif assegni
008000120222      *---------------------------------------------------------------*
008100120222     C     srass         BEGSR
008200120227     c                   eval      csmkey = csbnra
008300120227     c     csmkey        setll     tncsm03l
008400120222     c                   do        *hival
008500120227     c     csmkey        reade     tncsm03l
008600120222     c                   if        %eof(tncsm03l)
008700120222     c                   leave
008800120222     c                   end
008900120222     C                   MOVE      CSmidc        csbidc
009000120227     C                   MOVE      CSmkey        csbprg
009100120222     C                   MOVE      CSmNRA        csbnra
009200120222     C                   MOVE      CSmDTE        csbdte
009300120222     C                   MOVE      CSmABI        csbABI
009400120222     C                   MOVE      CSmCAB        csbCAB
009500120222     C                   MOVEL     CSmTPA        csbtpa
009600120222     C                   WRITE     Tncsbw00
009700120222     c                   enddo
009800120222     C*
009900120222     C                   ENDSR
010000030324     C*---------------------------------------------------------------*
010100030324     C     SRBAR         BEGSR
010200030324     C*---------------------------------------------------------------*
010300030324     C                   CLEAR                   DSSI95
010400030325     C* NEL FILE ESISTONO DUE RIFERIMENTI AL DESTINATARIO: 666 E INDIRIZZO
010500030325     C* ABITUALE
010600030324     c* no clienti 8888 o 9999
010700030324     C     §QUATR        IFne      8888
010800030324     C     §QUATR        andne     9999
010900030325     C* SE TROVO PRENDO DA FNSPE INDIRIZZO DELLA SPEDIZIONE
011000030324     C     KSPE          CHAIN     FNSPE03l
011100030324     C                   if        %found(fnspe03l)
011200030324     C                   MOVEL     SPERAG        csbrsm
011300030325     C                   MOVEL     SPEIND        csbinm
011400030324     C                   MOVEL     SPELOC        i95loc
011500030324     C                   MOVEL     SPECAP        i95cap
011600030324     C                   MOVEL     SPEPRO        i95prv
011700030324     C                   MOVEL     SPENAZ        i95nar
011800030327     C* RICHIAMO IL PGM TISI95R PER REPERIRE LA ZONA
011900030327     C                   MOVEL     '3'           I95TCN
012000070119     C                   MOVE      *date         dtaiso
012100070119     C                   MOVE      dtaiso        i95dat
012200070119     C*                  MOVEL     csbaas        I95DAT
012300070119     C*                  MOVE      csbmgs        I95DAT
012400030327     C                   CALL      'TISI95R'
012500030327     C                   PARM                    DSSI95
012600030327     C     O95ERR        IFeq      *BLANKS
012700030327     c                   eval      csbzsc = o95znc
012800030327     C                   eval      csbpoa = o95lna
012900030327     C                   eval      csblom = o95loc
013000030327     C                   eval      csbcam = o95cap
013100030327     C                   eval      csbprm = o95prv
013200030327     C                   eval      csbnzm = o95nar
013300030327     c                   end
013400030327     C                   end
013500030325     C* PRENDO DA CNACO l'indirizzo abituale
013600030324     C                   CLEAR                   TIBS69
013700030324     C                   CLEAR                   CNACO
013800030324     C                   CLEAR                   CNIND
013900030324     C                   CLEAR                   CNCLP
014000030324     C                   CLEAR                   FNCLS
014100030324     C                   MOVEL     ' '           I69TLA
014200030324     C                   MOVEL     KNSIF         I69SIF
014300030324     C                   Z-ADD     0             I69KCC
014400030326     C                   Z-ADD     csbcdi        I69KAC
014500030326     C                   Z-ADD     csbcdi        I69KIN
014600030326     C                   Z-ADD     csbcdi        I69KCP
014700030324     C                   CALL      'TIBS69R'
014800030324     C                   PARM                    TIBS69
014900030324     C                   PARM                    CNACO
015000030324     C                   PARM                    CNIND
015100030324     C                   PARM                    CNCLP
015200030324     C                   PARM                    FNCLS
015300030324     C     O69ERR        IFEQ      *BLANK
015400030327     C* SE NON ESISTE 666 prendo i dati anagrafici del destinatario
015500030327     C* della spedizione dall'anagrafico
015600030327     c                   if        i95loc = ' '
015700030620     C                   movel(P)  ACORAG        csbrsm
015800030620     C                   MOVEL(p)  INDVIA        csbinm
015900030620     C                   MOVEL(p)  INDCIT        csblom
016000030324     C     INDCAE        IFNE      *BLANK
016100030620     C                   MOVEL(p)  INDCAE        csbcam
016200030324     C                   ELSE
016300030620     C                   MOVEL(p)  INDCAP        csbcam
016400030324     C                   END
016500030620     C                   MOVEL(p)  INDPRV        csbprm
016600030620     C                   MOVEL(p)  INDSTA        csbnzm
016700030327     c                   eval      csbzsc = 0
016800030327     C                   movel     csbcdi        csbpoa
016900030327     c                   end
017000030325     C* RIEMPO COMUNQUE I DATI DELl'indirizzo ABITUALE
017100030620     C                   MOVEL(p)  ACORAG        csbrsc
017200030620     C                   MOVEL(p)  INDVIA        csbinD
017300030620     C                   MOVEL(p)  INDCIT        csbloD
017400030325     C     INDCAE        IFNE      *BLANK
017500030620     C                   MOVEL(p)  INDCAE        csbcaD
017600030325     C                   ELSE
017700030620     C                   MOVEL(p)  INDCAP        csbcaD
017800030325     C                   END
017900030620     C                   MOVEL(p)  INDPRV        csbprD
018000030620     C                   MOVEL(p)  INDSTA        csbnZD
018100030327     c                   else
018200030327     c                   eval      o95err = o69err
018300030324     C                   END
018400030324     c* per 8888 o 9999 leggo tncsa01l
018500030324     C                   ELSE
018600030324     C     Knsp          CHAIN     TNCSA01l
018700030324     c                   if        %found(tncsa01l)
018800030327     c                   eval      csbzsc = 0
018900030327     C                   movel     csbcdi        csbpoa
019000030324     C                   MOVEL     CSARSD        csbrsm
019100030324     C                   MOVEL     CSAVID        csbinm
019200030326     C                   MOVEL     CSARSD        csbrsc
019300030325     C                   MOVEL     CSAVID        csbinD
019400030325     C                   MOVEL     CSACID        csblod
019500030325     C                   MOVEL     CSACAD        csbcad
019600030325     C                   MOVEL     CSAPRM        csbprd
019700030325     C                   MOVEL     CSANZM        csbnzd
019800030327     C                   MOVEL     CSACID        csblom
019900030327     C                   MOVEL     CSACAD        csbcam
020000030327     C                   MOVEL     CSAPRM        csbprm
020100030327     C                   MOVEL     CSANZM        csbnzm
020200030327     c                   else
020300030327     c                   eval      o95err = '1'
020400030324     C                   END
020500030324     C                   END
020600030326     c* se non trova anagrafico non scrivo il record sul file FIVABCAF
020700030327     c                   if        o95err = *blanks
020800030327     c                   z-add     1             a
020900030327     c     csbpoa        lookup    fil(a)                                 96
021000030327     c                   if        *in96
021100030327     c                   eval      csbtfa = rag(a)
021200030327     c                   else
021300030327     c                   eval      o95err = '1'
021400030327     c                   end
021500030326     c                   end
021600030324     C*
021700030324     C                   ENDSR
021800030324     C*---------------------------------------------------------------*
021900030326     C     stampa        BEGSR
022000030324     C*---------------------------------------------------------------*
022100030326     c*
022200030326     c   of              except    testa
022300030326     c                   setoff                                       of
022400030326     c                   except    errore
022500030326     C*
022600030326     C                   ENDSR
022700030326     C*---------------------------------------------------------------*
022800030326     C     *INZSR        BEGSR
022900030326     C*---------------------------------------------------------------*
023000030326     C     *ENTRY        PLIST
023100030324     C                   PARM                    KPJBA
023200030324     C* DEFINIZIONE CHIAVI
023300030324     C     KCSB          KLIST
023400030324     C                   KFLD                    §TPI
023500030324     C                   KFLD                    §BNA
023600030324     C     KSPE          KLIST
023700030324     C                   KFLD                    spefls
023800030324     C                   KFLD                    csbcdi
023900030324     C                   KFLD                    COD               3
024000030324     C                   MOVE      '666'         COD
024100030324     C                   MOVEL     'L'           spefls
024200030324     C*
024300030324     C     KNSP          KLIST
024400030324     C                   KFLD                    CSBAAS
024500030324     C                   KFLD                    CSBLNP
024600030324     C                   KFLD                    CSBNRS
024700030324     C                   KFLD                    CSBNSP
024800030324     C*
024900030324     C                   MOVEL     'M'           §TPI              1
025000030324     C                   MOVE      *ZEROS        §BNA
025100030326     c                   seton                                        of
025200030324     C*
025300030324     C     *LIKE         DEFINE    CSBBNA        §BNA
025400030324     C*
025500030324     c                   exsr      carcdc
025600030324     C                   ENDSR
025700030324     C**-------------------------------------------------------
025800030324     C*+ CARICAMENTO RAGGRUPPAMENTI FILIALE 1°LIVELLO CONTABIL.
025900030324     C**-------------------------------------------------------
026000030324     C     CARCDC        BEGSR
026100030324     C                   SETOFF                                       96
026200030324     C                   clear                   fil
026300030324     C                   clear                   rag
026400030324     C     *LOVAL        SETLL     AZORG
026500030324     C                   DO        999           A                 4 0
026600030324     C                   READ      AZORG                                  96
026700030324     C  N96ORGFAG        IFEQ      'F'
026800030324     C     ORGFAG        OREQ      'A'
026900030324     C                   MOVE      ORGFIL        FIL(a)
027000030617      *  Reperisco terminal di arrivo
027100030617     C                   CLEAR                   FNLV55
027200030617     C* Imposto i dati in LV55
027300030617     C                   MOVE      *date         dtaiso
027400030617     C                   MOVE      dtaiso        D55DRF
027500030617     C                   MOVEL     ORGFIL        D55LIN
027600030618     c* forzo Bologna come terminal x controllare le eccezioni xchè 102 non
027700030618     c* è detto che esista
027800030618     C                   MOVE      001           D55Lnp
027900030617     C                   CALL      'FNLV55R'
028000030617     C                   PARM                    FNLV55
028100030617     C                   MOVE      D55TFA        RAG(a)
028200030324     C                   END
028300030324     C  N96              END
028400030324     C*
028500030324     C                   ENDSR
028600030326     oqsysprt   e            testa
028700030326     o                       udate               +0 '  /  /  '
028800030326     o                                           70 'ERRORE REPERIMETO ANAGRAFI-
028900030326     o                                              CO CLIENTI'
029000030326     o                                           +5 'TNSC77R'
029100030326     o          e            errore
029200030326     o                                           +0 'Anno'
029300030326     o                       csbaas              +1
029400030326     o                                           +1 'lnp'
029500030326     o                       csblnp              +1
029600030326     o                                           +1 'serie'
029700030326     o                       csbnrs        2     +1
029800030326     o                                           +1 'numero'
029900030326     o                       csbnsp        2     +1
030000030326     o                                           +1 'cliente'
030100030326     o                       csbcdi        2     +1
