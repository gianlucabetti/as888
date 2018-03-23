000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200941021      * FNLR93R *----------------------------------------------------*
000300911119      *-----------------*                                            *
000400941021      *        STAMPA ELENCO SPEDIZIONI CON RICHIESTA NOTA CREDITO   *
000500911119      *--------------------------------------------------------------*
000600020925     FFNARB71L  UF   E           K DISK
000700991012     FFIAR601L  IF   E           K DISK
000800991013     FFIARI01L  IF   E           K DISK
000900060214     FFiAR401L  IF   E           K DISK
001000920312     FAZORG01L  IF   E           K DISK
001100050309     FTIGCP21L  IF   E           K DISK
001200931118     FTABEL00F  IF   E           K DISK
001300011130     FTNTBE01L  IF   E           K DISK
001400040317     Ffnncr00f  o    E             DISK
001500060511     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
001600060511     F                                     USROPN
001700040506     F*ET132    O    F  132        PRINTER OFLIND(*INOA)
001800040506     F*                                    USROPN
001900900515      *
002000900606     D FLU             S              1    DIM(32)                              FLAG PAR.UTENT
002100920317     D CMDP            S              1    DIM(60) CTDATA PERRCD(60)            PRINTER OVRPRTF
002200920316     D CMN             S              1    DIM(70) CTDATA PERRCD(70)            OVRPRTF N.ACCR
002300931118     D CCA             S              1    DIM(20)                              COD.CONS.ANIOMAL
002400931118     D DCCA            S             14    DIM(20)                              DESCR.CONS.ANOM.
002500940627     D*
002600940627     D CEI             S              1    DIM(50)                              CODICI DI ESENZ.
002700020507     D ESE             S              3    DIM(50)                              ARTICOLI ESENTI
002800941021     D*
002900941021     D T3A             S              2    DIM(40)                              TAB. COD. BOLLA
003000941021     D D3A             S             89    DIM(40)                              DS3A TIPO BOLLA
003100991013     D CCV             S              3    DIM(200)                             TAB.DIVSE
003200991013     D DCV             S             89    DIM(200)                             DS  DIVISE
003300900515      *
003400941021     D WLBDA8          DS
003500941021     D  G08DAT                 1      8  0
003600941021     D  G08INV                 9     16  0
003700941021     D  G08ERR                17     17
003800941021     D  G08TGI                18     22  0
003900950118     D WLBDAT          DS
004000950118     D  G06DAT                 1      6  0
004100950118     D  G06INV                 7     12  0
004200950118     D  G06ERR                13     13
004300950118     D  G06TGI                14     18  0
004400050203     D ddatiute      e ds
004500050203     D azuteds       e ds                  extname(AZUTE00F)
004501060411     D TRul06ds      E DS
004502060420     D  lin                    1     90  0 dim(30) inz
004600050203     D tibs34ds      E DS                  inz
004700940627     D DSEI          E DS
004800941021     D DS3A          E DS
004900991013     D DSCV          E DS
005000011130     D DGED          E DS
005100991013     D YEURDS        E DS                  EXTNAME(YEURCODS)
005200900514     D KPJBA         E DS
005300900514     D  LIBSYS                92    100
005400900514     D  MSGERR               453    502
006000900516      *
006100920311     D PARAM           DS
006200920311     D  PFLE                   1      3  0
006300920311     D  PCUC                   4      6  0
006400920311     D  PNMI                   7     13  0
006500920320     D  PDMI                  14     19
006600920311      *
006700020911     D* DS PER TRUL33R - REPERIMENTO/AGGIORNAMENTO NUMERATORI SU AZNUM
006800020911     D TRUL33DS      E DS
006900941021     D                SDS
007000941021     D  §PGM                   1     10
007100020507     D CdIVA           S              4
007200020925     D kipp            S                   like(arbipp)
007300020925     c* mi metto in loop per ogni po
007400060411     c                   do        30            x                 3 0
007500040316     c                   if        lin(x) = 0
007600040316     c                   leave
007700020925     c                   else
007800040316     c                   z-add     lin(x)        kipp
007900020925     c                   end
008000020507
008100950118      * SE MANDATO INTROITO = 999999 PGM SOTTOMESSO DA CHIUSURA CASSA
008200950118      * PER AVERE ELENCO DELLE SPEDIZ. DIROTTATE .....
008300950118      * ALTRIMENTI SOTTOMESSO DA CHIUSURA OPERATORE  PER AVERE ELENCO
008400950118      * SPEDIZ. CHIUSE IN DISTINTA CON NOTA ACCR. FARE STAMPA MOD.RICH.
008500020925     C     KARB          SETLL     FNARB71L                           30
008600911119     C     *IN30         DOWEQ     '0'
008700020925     C     KARB          READE     FNARB71L                             1530    ALLOC=15 ON
008800920311     C  N30
008900920311     CANN15ARBFIP        IFEQ      'C'
009000930513     C     ARBFNC        ANDNE     'E'                                          GIA STAMPATO ELENCO
009100920320     C* SE NMI <> 999999 : STAMPO SEMPRE
009200920320     C* SE NMI = 999999  : TESTO LA DATA PASSATA
009300920320     C     ARBNMI        IFNE      *ALL'9'
009400920320     C     ARBNMI        OREQ      *ALL'9'
009500920320     C     ARBDMI        ANDEQ     §DMI
009600941021     C                   MOVE      *ZEROS        AR6NFT
009700941021     C                   MOVE      *ZEROS        AR6DFT
009800941021     C                   MOVE      *ZEROS        AR6IFT
009900991012     C                   MOVE      *ZEROS        AR6KSC
010000941021      * ANALIZZO LA BOLLA
010100941021     C                   CLEAR                   DS3A
010200941021     C                   Z-ADD     1             I
010300941021     C     ARBCBO        LOOKUP    T3A(I)                                 26
010400941021     C   26              MOVEL     D3A(I)        DS3A
010500941021     C   26              MOVEL     §3ADES        DTBL             20
010600941021     C  N26              MOVEL     *ALL'?'       DTBL
010700941021     C                   MOVEL     §3ATB1        PORTO             1            PORTO DELLA BOLLA
010800941021      * ASSEGNATO : PRENDO DATI TASSAZIONE 1° BOLLA
010900941021     C     PORTO         IFEQ      'A'
011000991012     C                   MOVEL     '1'           KTRC
011100991013     C     KAR6          CHAIN     FIAR601L                           22
011200991012     C                   SETON                                        32
011300941021     C                   END
011400941021     C     §3ATB2        IFNE      *BLANK
011500991012     C                   MOVEL     '2'           KTRC
011600991013     C     KAR6          CHAIN     FIAR601L                           22
011700991012     C                   SETON                                        37
011800941021     C                   END
011900920316     C                   SETOFF                                       3237
012000940627     C                   Z-ADD     0             TOTESE
012100940627     C                   Z-ADD     0             TOTIMP
012200940627     C                   Z-ADD     0             IMPIVA
012300991012     C* IMPOSTO DATI FATTURA
012400991013     C                   Z-ADD     AR6NFT        NUMFAT            7 0
012500991013     C                   MOVEL     AR6DIV        FATDIV            3
012600991013     C                   Z-ADD     AR6IFT        FATIMP           13 3
012700991013     C                   Z-ADD     AR6IFT        STORNO           12 3
012800991115     C                   Z-ADD     AR6IFT        STORN1           11 2
012900991013     C                   MOVE      FATIMP        COMO3             3 0
013000991115     C                   MOVE      STORN1        COMO2             2 0
013100991013     C     COMO3         COMP      0                                      10
013200991115     C     COMO2         COMP      0                                      16
013300991012     C     AR6KSC        IFEQ      0
013400991013     C                   Z-ADD     ARBKSC        CLIENT            7 0
013500991012     C                   ELSE
013600991012     C                   Z-ADD     AR6KSC        CLIENT
013700991012     C                   END
013800991013     C                   Z-ADD     AR6FIV        FILFAT            3 0
013900950118     C                   Z-ADD     AR6DFT        G06INV
014000991013     C                   Z-ADD     AR6ALI        ALIQ              3 1
014100020507     C                   EVAL      CdIVA = %EDITC(Aliq:'2')
014200920316     C* CALCOLO LA DIFFERENZA DA STORNARE
014300991012     C                   MOVEL     'A'           KTIP
014400991012     C     KARI          CHAIN     FIARI01L                           20
014500991012     C     *IN20         DOWEQ     '0'
014600991012     C     AR6DIV        IFEQ      ARIDIV
014700991012     C                   SUB       ARIIPP        STORNO
014800991012     C                   SUB       ARIIMP        STORNO
014900991115     C                   SUB       ARIIPP        STORN1
015000991115     C                   SUB       ARIIMP        STORN1
015100991012     C                   ELSE
015200991012     C* Se la divisa di incasso è diversa converto importo già
015300991012     C* incassato nella divisa della bolla
015400991012     C                   CLEAR                   YEURDS
015500991012     C                   MOVEL     ARIDIV        YECDVI
015600991012     C                   Z-ADD     ARIIPP        YECIMI
015700991012     C                   ADD       ARIIMP        YECIMI
015800991012     C                   MOVEL     AR6DIV        YECDVO
015900991012     C                   MOVEL     'H'           YECTAR
016000991012     C                   CALL      'YEURCO'
016100991012     C                   PARM                    YEURDS
016200991012     C                   SUB       YECIMO        STORNO
016300991115     C                   SUB       YECIMO        STORN1
016400920316     C                   END
016500991012     C     KARI          READE     FIARI01L                               20
016600991012     C                   END
016700950118     C                   MOVE      *ZEROS        G06DAT
016800950118     C                   MOVEL     '3'           G06ERR
016900950118     C                   CALL      'XSRDAT'
017000950118     C                   PARM                    WLBDAT
017100950118     C                   MOVEL     G06DAT        DATAF             6 0
017200941021     C* CALCOLO L'IMPONIBILE SE NON è TASSATO DESTINO
017300941021     C     §3AFTD        IFNE      0
017400941021     C                   MOVE      *ZEROS        ALIQ
017500020507     C                   CLEAR                   CdIVA
017600941021     C                   END
017700940627     C                   MOVEL     ' '           TPALI             1
017800940627     C     ALIQ          IFNE      *ZEROS
017900940627     C     ALIQ          DIV       100           COMALI            4 3
018000920316     C     COMALI        ADD       1             COMALI
018100991012     C     STORNO        DIV       COMALI        COMIVA           14 5
018200991013     C                   MOVEL     AR6DIV        DIVIMP            3
018300991013     C                   Z-ADD     COMIVA        IMPONI           12 3
018400991115     C*  SE L'IMPORTO DELLA NOTA DI ACCREDITO SONO LE LIRE
018500991115     C*  NON DEVO GESTIRE I DECIMALI
018600991115     C     AR6DIV        IFEQ      'ITL'
018700991115     C                   MOVE      000           IMPONI
018800991115     C* ... SE EURO VERIFICO SE IMPOSTATA 3 CIFRA DECIMALE
018900991115     C*     PER VEDERE SE E COME ARRONATONDARE
019000991115     C                   ELSE
019100991115     C                   MOVE      COMIVA        COMO1             1 0
019200991115     C                   MOVE      0             IMPONI
019300040511     C*    COMO1         IFGT      5
019400040511     C*                  ADD       0,01          IMPONI
019500040511     C*                  END
019600991115     C                   END
019700991013     C                   MOVE      IMPONI        COMO3
019800991013     C     COMO3         COMP      0                                      11
019900991115     C                   Z-ADD     IMPONI        TOTIMP           12 3
020000991013     C                   MOVE      TOTIMP        COMO3
020100991013     C     COMO3         COMP      0                                      12
020200991013     C     STORNO        SUB       IMPONI        IMPIVA           12 3
020300991013     C                   MOVE      IMPIVA        COMO3
020400991013     C     COMO3         COMP      0                                      13
020500920316     C                   ELSE
020600991013     C                   Z-ADD     STORNO        TOTESE           12 3
020700991013     C                   MOVE      TOTESE        COMO3
020800991013     C     COMO3         COMP      0                                      14
020900920316     C                   Z-ADD     STORNO        IMPONI
021000940627     C                   Z-ADD     1             XX
021100940627     C     TPALI         LOOKUP    CEI(XX)                                41
021200940627     C     *IN41         IFEQ      '1'
021300020507     C                   CLEAR                   ALIQ
021400020507     C                   EVAL      CdIVA = Ese(xx)
021500940627     C                   END
021600940627     C                   MOVEL     'E'           TPALI
021700920316     C                   END
021800930924      *
021900930924      * ESEGUO REPERRIMENTO DATI GIACENZA
022000930924     C                   MOVE      *ZERO         GCPAGC
022100930924     C                   MOVE      *ZERO         GCPFGC
022200930924     C                   MOVE      *ZERO         GCPNGC
022300930924     C                   MOVE      ARBAAS        GCPAAS
022400930924     C                   MOVE      ARBLNP        GCPLNP
022500930924     C                   MOVE      ARBNRS        GCPNRS
022600930924     C                   MOVE      ARBNSP        GCPNSP
022700050309     C     KGCA          CHAIN     TIGCP21L                           21
022800920316     C*
022900931119     C*  DECODIFICO TIPO CONSEGNA ANOMALA
023000931118     C                   Z-ADD     1             W                 2 0
023100931118     C                   MOVEL     *BLANKS       DECC
023200931118     C     ARBCCA        LOOKUP    CCA(W)                                 40
023300991013     C   40              MOVE      DCCA(W)       DECC             11
023400931118     C*
023500060511     C  N90              EXCEPT    TESTA
023600060511     C  N90              SETON                                        90
023700060511     C   OF              EXCEPT    TESTA
023800941021     C*
023900941021     C* PRENDO 2° RAGIOSNE SOCIALE DESTINATARIO
024000941021     C                   MOVE      'D'           §TRC                           TIPO REC= DESTIN
024100060214     C     KAR4          CHAIN     FiAR401L                           43
024200941021     C  N43              MOVE      AR4NOT        AR4RD2
024300941021     C   43              MOVEL     *BLANK        AR4RD2
024400991013     C                   MOVEL     ARBRSD        RSD20            20
024500941021     C                   MOVE      ARBAAS        AAS2              2 0
024600941021     C                   MOVE      GCPAGC        AGC2              2 0
024700991115     C                   Z-ADD     AR6IFT        IMPFAT           11 2
024800991115     C                   MOVE      IMPFAT        COMO2             2 0
024900991115     C     COMO2         COMP      0                                      17
025000060511     C                   EXCEPT    RIGA
025100991118     C                   MOVEL     'S'           WSTP              1
025200911119      *
025300930507      * STAMPO IL MODULO RICHIESTA NOTA ACCREDITO SE NON L'HO
025400930507      * GIA FATTO DA FLFG20R
025500930513     C     ARBFNC        IFEQ      ' '
025600930507     C                   EXSR      PGMNAC
025700920320     C                   END
025800930507      * AGGIORNO FLAG DI STAMPA ELENCO NOTE ACCREDITO
025900930513     C                   MOVE      'E'           ARBFNC
026000941021     C                   EXCEPT    AGG0
026100930507     C                   END
026200920320     C                   END
026300920311     C                   END
026400020925     C                   ENDdo
026500930409      *
026600991118     C     WSTP          IFEQ      'S'
026700060511     C                   EXCEPT    FINE
026800991118     C                   END
026900910521     C                   SETON                                        LR
027000920316     C*---------------------------------------------------------------*
027100920316     C*   P G M N A C                                                 *
027200920316     C*---------------------------------------------------------------*
027300920316     C     PGMNAC        BEGSR
027400020911     C*  PRENDO NR. RICH. NOTA ACCREDITO
027500040317     c*m                 clear                   trul33ds
027600040317     c*m                 move      'L'           i33tla
027700040317     c*m                 z-add     14            i33cnu
027800040317     c*m                 z-add     filfat        i33po1
027900040317     c*m                 z-add     1             i33num
028000040317     c*m                 movel     trul33ds      kpjbu
028100040317     c*m                 call      'TRUL33R'
028200040317     c*m                 PARM                    kpjba
028300040317     c*m                 movel     kpjbu         trul33ds
028400040317     c*m                 z-add     o33nri        §notac            7 0
028500040317     C*m
028600920316     C     ARBLNA        CHAIN     AZORG01L                           21
028700960927     C  N21              MOVEL     ORGDES        DLNA             20
028800920316     C   21              MOVEL     *ALL'?'       DLNA
028900991013     C                   MOVE      STORNO        COMO3
029000991013     C     COMO3         COMP      0                                      15
029100011130      * VERIFICO SE GLI IMPORTI SONO IN MONETA DI CONTO CONTABILE
029200011130      * SE NON SONO COSÌ LI CONVERTO PER STAMPARE IL DOPPIO IMPORTO
029300011130     C                   CLEAR                   WFATIM
029400011130     C                   SETOFF                                       6061
029500011130     C     FATDIV        IFNE      §GEDMC
029600011130     C                   CLEAR                   YEURDS
029700011130     C                   MOVEL     FATDIV        YECDVI
029800011130     C                   Z-ADD     FATIMP        YECIMI
029900011130     C                   MOVEL     §GEDMC        YECDVO
030000011130     C                   MOVEL     'H'           YECTAR
030100011130     C                   CALL      'YEURCO'
030200011130     C                   PARM                    YEURDS
030300011130     C                   Z-ADD     YECIMO        WFATIM           13 3
030400011130     C                   SETON                                        60
030500011130     C                   ENDIF
030600011130      *
030700011130     C                   CLEAR                   WIMPON
030800011130     C     DIVIMP        IFNE      §GEDMC
030900011130     C                   CLEAR                   YEURDS
031000011130     C                   MOVEL     DIVIMP        YECDVI
031100011130     C                   Z-ADD     IMPONI        YECIMI
031200011130     C                   MOVEL     §GEDMC        YECDVO
031300011130     C                   MOVEL     'H'           YECTAR
031400011130     C                   CALL      'YEURCO'
031500011130     C                   PARM                    YEURDS
031600011130     C                   Z-ADD     YECIMO        WIMPON           13 3
031700011130     C                   SETON                                        61
031800011130     C                   ENDIF
031900011130      *
032000040506     C*                  EXCEPT    NACTES
032100040506     C*                  EXCEPT    NAC1
032200040506     C*                  EXCEPT    NACTE2
032300040506     C*                  EXCEPT    NACSPE
032400040506     C*                  EXCEPT    NACSAL
032500040506     C*                  EXCEPT    NACFIR
032600040317      * scrittura file NCR
032700040317     c                   clear                   fnncr000
032800040317     c                   move      '201'         nctsocieta
032900040317     c                   z-add     arblna        nctfil
033000040317      * anomalia chiusura con CA
033100040317     c                   if        arbcca = '5'
033200040317     c                   z-add     046           nctpocompl
033300040317     c                   move      'C'           ncrrna
033400040317     c                   move      '5'           ncrcmn
033500040317     c                   move      '0'           nchricompl
033600040317     c                   else
033700040317     c                   z-add     arblnp        nctpocompl
033800040317     c                   move      'D'           ncrrna
033900040317     c                   move      '0'           ncrcmn
034000040317     c                   move      '1'           nchricompl
034100040317     c                   endif
034200040317
034300040317     c                   move      amgudt        nctddc
034400040317     c                   if        ar6dft > 0
034500040317     c                   move      ar6dft        nctdrp
034600040317     c                   end
034700040317     c                   z-add     ar6nft        nctnrp
034800040317     c                   z-add     ar6fiv        nctfpn
034900040317     c                   z-add     ar6ift        nctimr
035000040317     c                   move      *zeros        nctksc
035100040317     c                   move      client        nctksc
035200040317     c                   move      *zeros        nctscf
035300040317     c                   move      client        nctscf
035400040317     c                   move      knmus         nctutente
035500040317     c                   if        fatimp = storno
035600040317     c                   move      'I '          ncttnr
035700040317     c                   else
035800040317     c                   move      'B '          ncttnr
035900040317     c                   end
036000040317      * storno spese giacenza
036100040318     c                   if        ar6ali > 0
036200040318     c                   z-add     totimp        ncrim2
036300040318     c                   else
036400040318     c                   z-add     totese        ncrim2
036500040318     c                   end
036600040317     c                   z-add     ar6ali        ncrali
036700040317     c                   z-add     arblnp        ncrflr
036800040317     c                   z-add     arbaas        ncraassto
036900040317     c                   z-add     arblnp        ncrlnpsto
037000040317     c                   z-add     arbnrs        ncrnrssto
037100040317     c                   z-add     arbnsp        ncrnspsto
037200040317
037300040317     c                   move      ar6cei        nchcei
037400040317     c                   eval      nchorigine = 'STORNO ASSEGNATO DA CHIUSURA -
037500040317     C                             OPERATORE'
037600040317     c                   write     fnncr000
037700040317     C*
037800920316     C                   ENDSR
037900941021      *---------------------------------------------------------------*
038000941021     C     *INZSR        BEGSR
038100941021     C     *ENTRY        PLIST
038200941021     C                   PARM                    KPJBA
038300941021     C                   MOVEL     KPJBU         PARAM
038400941021      *
038500941021     C                   MOVEL     *ALL'-'       TRATT           132
038600050203     C                   Z-ADD     1             CODUT             1 0
038700050203     c* dati utente
038800050203     c     *dtaara       define    §azute        azuteds
038900050203     c     *dtaara       define    §datiute      ddatiute
039000050203     C                   in(E)     *dtaara
039100050203     C                   IF        %Error  or  RSUT = *blanks
039200050203     C                   call      'TIBS34R'
039300050203     C                   parm                    Tibs34Ds
039400050203     C                   in        *dtaara
039500050203     c                   ENDIF
039600941021     C*§§
039700941021     C                   TIME                    WHHUDT           14 0
039800941021     C                   MOVEL     WHHUDT        ORA               6 0
039900941021     C                   MOVE      WHHUDT        G08DAT
040000941021     C                   MOVE      *ZEROS        G08INV
040100941021     C                   MOVEL     *BLANK        G08ERR
040200941021     C                   CALL      'XSRDA8'
040300941021     C                   PARM                    WLBDA8
040400941021     C                   Z-ADD     G08INV        AMGUDT            8 0
040500941021     C                   Z-ADD     G08DAT        GMAUDT            8 0
040600941021     C*§§
040700950118     C     PNMI          IFEQ      *ALL'9'
040800950118     C                   MOVEL     AMGUDT        §DMI
040900950118     C                   MOVE      PDMI          §DMI              8 0
040901060411      * ricarico la £6 se veniva cambiato il P.O aveva i dati errati
040902060411     c                   clear                   trul06ds
040903060411     C                   MOVEL     '£6'          D06COD
040904060529     C                   MOVEL     pfle          D06KEY
040905060411     C                   MOVEL     TRul06ds      KPJBU
040906060411     C                   CALL      'TRUL06R'
040907060411     C                   PARM                    KPJBA
040908060411     C                   MOVEL     KPJBU         TRul06ds
041900020925     c                   else
042000020925     c* se mandato int. non è *all'9' loop solo con po passato
042100040316     c                   move      pfle          lin(1)
042200020925     C                   END
042300941021     C*
042400941021     C     KBOL          KLIST
042500941021     C                   KFLD                    ARBAAS
042600941021     C                   KFLD                    ARBLNP
042700941021     C                   KFLD                    ARBNRS
042800941021     C                   KFLD                    ARBNSP
042900941021      *
043000941021      *- CHIAVE X TABEL00F -------------------------------------------*
043100941021     C     KTAB          KLIST
043200941021     C                   KFLD                    CODUT
043300941021     C                   KFLD                    CODTAB            2
043400941021     C                   KFLD                    KEYTAB            8
043500941021     C     KTAB1         KLIST
043600941021     C                   KFLD                    CODUT
043700941021     C                   KFLD                    CODTAB
043800941021      *
043900941021     C     KARB          KLIST
044000020925     C                   KFLD                    kipp
044100020925     C                   KFLD                    PNMI
044200941021     C                   KFLD                    PCUC
044300941021     C*
044400941021     C*  CHIAVE PER POSIZIONAMENTO SU ARCHIVIO GIACENZE
044500941021     C     KGCA          KLIST
044600941021     C                   KFLD                    GCPAAS
044700941021     C                   KFLD                    GCPLNP
044800941021     C                   KFLD                    GCPNRS
044900941021     C                   KFLD                    GCPNSP
045000941021     C*
045100941021     C     KSPE          KLIST
045200941021     C                   KFLD                    ARBAAS
045300941021     C                   KFLD                    ARBLNP
045400941021     C                   KFLD                    ARBNRS
045500941021     C                   KFLD                    ARBNSP
045600941021     C*
045700941021     C     KAR4          KLIST
045800941021     C                   KFLD                    ARBAAS
045900941021     C                   KFLD                    ARBLNP
046000941021     C                   KFLD                    ARBNRS
046100941021     C                   KFLD                    ARBNSP
046200941021     C                   KFLD                    §TRC
046300991013     C     KAR6          KLIST
046400991013     C                   KFLD                    ARBAAS
046500991013     C                   KFLD                    ARBLNP
046600991013     C                   KFLD                    ARBNRS
046700991013     C                   KFLD                    ARBNSP
046800991013     C                   KFLD                    KTRC              1
046900991013     C     KARI          KLIST
047000991013     C                   KFLD                    ARBAAS
047100991013     C                   KFLD                    ARBLNP
047200991013     C                   KFLD                    ARBNRS
047300991013     C                   KFLD                    ARBNSP
047400991013     C                   KFLD                    KTIP              1
047500011130     C*
047600011130     C     KTBE          KLIST
047700011130     C                   KFLD                    TBECOD
047800011130     C                   KFLD                    TBEKE1
047900011130     C                   KFLD                    TBEKE2
048000941021      *
048100941021     C     *LIKE         DEFINE    AR4TRC        §TRC
048200941021     C     *LIKE         DEFINE    AR4NOT        AR4RD2
048300991013      *
048400991013     C* AGGANCIO LA CV
048500991013     C*
048600991013     C                   MOVEL     'CV'          CODTAB
048700020507     C                   CLEAR                   ESE
048800991013     C                   MOVEA     *BLANKS       CEI
048900991013     C*
049000991013     C     KTAB1         CHAIN     TABEL00F                           21
049100991013     C                   Z-ADD     1             XX                2 0
049200991013     C     *IN21         DOWEQ     '0'
049300991013     C     XX            ANDLE     200
049400991013     C                   MOVEL     TBLUNI        DCV(XX)
049500991013     C                   MOVEL     TBLKEY        CCV(XX)
049600991013     C                   ADD       1             XX
049700991013     C     KTAB1         READE     TABEL00F                               21
049800991013     C                   END
049900941021      *
050000941021     C* AGGANCIO LA EI
050100941021     C*
050200941021     C                   MOVEL     'EI'          CODTAB
050300020507     C                   CLEAR                   ESE
050400941021     C                   MOVEA     *BLANKS       CEI
050500941021     C*
050600941021     C     KTAB1         CHAIN     TABEL00F                           21
050700941021     C                   Z-ADD     1             XX                2 0
050800941021     C     *IN21         DOWEQ     '0'
050900941021     C     XX            ANDLE     50
051000941021     C                   MOVEL     TBLUNI        DSEI
051100020507     C                   MOVEL     §EIJEI        ESE(XX)
051200941021     C                   MOVEL     TBLKEY        CEI(XX)
051300941021     C                   ADD       1             XX
051400941021     C     KTAB1         READE     TABEL00F                               21
051500941021     C                   END
051600941021      *- CARICO DESCRIZIONI CONS.ANOMALE     ---------------------*
051700941021     C                   MOVEL     '7O'          CODTAB
051800941021     C                   MOVE      *BLANKS       KEYTAB
051900941021     C                   Z-ADD     0             I
052000941021     C     KTAB          SETLL     TABEL00F                           22
052100941021     C     *IN22         DOWEQ     '0'
052200941021     C     KTAB1         READE     TABEL00F                               22
052300941021     C     *IN22         IFEQ      '0'
052400941021     C     I             ANDLT     20
052500941021     C                   ADD       1             I                 3 0
052600941021     C                   MOVEL     TBLKEY        CCA(I)
052700941021     C                   MOVEL     TBLUNI        DCCA(I)
052800941021     C                   END
052900941021     C                   END
053000941021      * CARICO TUTTI I TIPI BOLLA    TAB.3A
053100941021     C                   MOVEL     '3A'          CODTAB
053200941021     C                   MOVE      *BLANKS       KEYTAB
053300941021     C                   MOVE      *ZEROS        I
053400941021     C     KTAB          SETLL     TABEL00F
053500941021     C     *IN22         DOUEQ     '1'
053600941021     C     KTAB1         READE     TABEL00F                               22
053700941021     C  N22              ADD       1             I
053800941021     C  N22              MOVEL     TBLKEY        T3A(I)
053900941021     C  N22              MOVEL     TBLUNI        D3A(I)
054000941021     C                   END
054100941021      *
054200941021      *- APRO FILE DI STAMPA -----------------------------------------*
054300060511     C                   Z-ADD     60            LUNCMD           15 5
054400060511     C                   CALL      'QCMDEXC'
054500060511     C                   PARM                    CMDP
054600060511     C                   PARM                    LUNCMD
054700060511     C                   OPEN      QSYSPRT
054800060511     C                   SETON                                        OF
054900941021     C* OVRPRTF  N. ACCREDITO
055000060511     C*                  Z-ADD     70            LUNCMD           15 5
055100060511     C*                  CALL      'QCMDEXC'
055200060511     C*                  PARM                    CMN
055300060511     C*                  PARM                    LUNCMD
055400060511     C*                  OPEN      LET132
055500941021      *
055600011130     C* REPERISCO LA DIVISA NELLA MONETA CORRENTE
055700011130     C                   MOVEL     'GED'         TBECOD
055800011130     C                   MOVEL     *BLANKS       TBEKE1
055900011130     C                   MOVEL     *BLANKS       TBEKE2
056000011130     C                   MOVEL     '1'           TBEKE1
056100011130     C     KTBE          CHAIN     TNTBE01L                           21
056200011130     C  N21              MOVEL     TBEUNI        DGED
056300941021     C                   ENDSR
056400911119      *
056500900606      *----------------------------------------------------------------
056600941021     OFNARB000  E            AGG0
056700920325     O                       ARBFNC
056800060511     OQSYSPRT   E            TESTA          1 01
056900060511     O                       RSUT                20
057000060511     O                       SIMFEL            +  1
057100060511     O                                           58 'ELENCO SPEDIZIONI'
057200060511     O                                         +  1 'CON RICHIESTA DI NOTA'
057300060511     O                                         +  1 'CREDITO'
057400060511     O                       GMAUDT            +  5 '  /  /    '
057500060511     O                       §PGM              +  3
057600060511     O                                          128 'PAG.'
057700060511     O                       PAGE               132
057800060511     O          E            TESTA          1
057900060511     O                       KNSIF             +  0
058000060511     O                       KNMUS             +  1
058100060511     O                       ORA                103 '  :  :  '
058200911106      *
058300060511     O          E            TESTA          1
058400060511     O                                         +  1 'PUNTO OPERATIVO:'
058500060511     O                       PFLE              +  1
058600060511     O                                         +  2 'OPERATORE:'
058700060511     O                       PCUC              +  1
058800060511     O                                         +  2 'NR.MANDATO:'
058900060511     O                       PNMI          Z   +  1
059000060511     O          E            TESTA          1
059100060511     O                       TRATT             +  0
059200060511     O          E            TESTA          1
059300060511     O                                           19 'S P E D I Z I O N E'
059400060511     O                                           55 'DISTIN.'
059500060511     O                                           81 '   IMPORTO'
059600060511     O                                           89 'REG'
059700060511     O                                          119 'DATI GIACENZA'
059800911106      *
059900060511     O          E            TESTA          1
060000060511     O                                            3 'LNP'
060100060511     O                                            7 'LNA'
060200060511     O                                           10 'AA'
060300060511     O                                           13 'NR'
060400060511     O                                           21 'SPEDIZ.'
060500060511     O                                           34 'DESTINATARIO'
060600060511     O                                           50 'CONSEG.'
060700060511     O                                           55 'DATA'
060800060511     O                                           67 'NUMERO'
060900060511     O                                           81 'FATTURA'
061000060511     O                                           89 'IVA'
061100060511     O                                           99 'IMP. STORNO'
061200060511     O                                          105 'AA'
061300060511     O                                          110 'FGC'
061400060511     O                                          117 'NUMERO'
061500060511     O                                          132 'CONS.ANOMALA'
061600911106      *
061700060511     O          E            TESTA          1
061800060511     O                       TRATT             +  0
061900911120      *
062000060511     O          E            RIGA           1
062100060511     O                       ARBLNP               3
062200060511     O                       ARBLNA               7
062300060511     O                       AAS2                10
062400060511     O                       ARBNRS        Z     13
062500060511     O                       ARBNSP        Z     21
062600060511     O                       RSD20               42
062700060511     O                       ARBNDC        Z     50
062800060511     O                       DATAF               59 '  /  /  '
062900060511     O                       AR6NFT        Z     67
063000060511     O                       AR6DIV              71
063100060511     O                       IMPFAT        3     85
063200060511     O               17                          85 '   '
063300060511     O                       AR6FIV              89
063400060511     O                       STORN1        3    103
063500060511     O               16                         103 '   '
063600060511     O                       AGC2          Z    106
063700060511     O                       GCPFGC        Z    110
063800060511     O                       GCPNGC        Z    118
063900060511     O                       ARBCCA             120
064000060511     O                       DECC               132
064100060511     O          E            FINE           1
064200060511     O                                         + 40 '** - F I N E    S T A M'
064300060511     O                                         +  1 'P A - **'
064400040506     O*ET132    E            NACTES         1 01
064500040506     O*                      §PGM              +  1
064600040506     O*                      GMAUDT            +  1 '  /  /    '
064700040506     O*                      ORA               +  1 '  :  :  '
064800040506     O*         E            NACTES         2  8
064900040506     O*                                        + 30 'SPETT.'
065000040506     O*                                        +  1 '________________________'
065100040506     O*         E            NACTES         2
065200040506     O*                                        + 30 'ATT.NE'
065300040506     O*                                        +  1 '________________________'
065400040506     O*         E            NACTES         1
065500040506     O*                                        +  1 'PUNTO OPERATIVO DI'
065600040506     O*         E            NACTES         2
065700040506     O*                      DLNA              +  1
065800040506     O*                                          60 'CLIENTE DI FATTURAZIONE:'
065900040506     O*                                        +  1 '_______'
066000040506     O*         E            NACTES         1
066100040506     O*                      ARBRSD              64
066200040506     O*         E            NACTES         1
066300040506     O*                      AR4RD2              64
066400040506     O*         E            NACTES         1
066500040506     O*                      ARBIND              64
066600040506     O*         E            NACTES         2
066700040506     O*                      ARBCAD              38
066800040506     O*                      ARBLOD            +  1
066900040506     O*                      ARBPRD            +  1
067000040506     O*                      ARBNZD            +  1
067100040506     O*         E            NACTES      0  1
067200040506     O*                                        +  1 'CLIENTE DI BOLLETTAZIONE'
067300040506     O*                                        +  0 ':'
067400040506     O*                                          56 'TIPO DI PAGAMENTO:  '
067500040506     O*                                        +  1 'I   S   B'
067600040506     O*         E            NACTES         2
067700040506     O*                      CLIENT        Z   +  1
067800040506     O*         E            NACTES      0  0
067900040506     O*                                        +  1 'RICHIESTA NOTA ACCREDITO'
068000040506     O*                                        +  1 '- NR.'
068100040506     O*                      filfat            +  1
068200040506     O*                      SIMFEL            +  1
068300040506     O*                                        +  0 '/'
068400040506     O*                      §NOTAC        Z   +  0
068500040506     O*                                        +  1 'DEL'
068600040506     O*                      GMAUDT            +  1 '  /  /    '
068700040506     O*                                        +  1 'COMP. __/__'
068800040506     O*         E            NACTES         2
068900040506     O*                                        +  1 'RICHIESTA NOTA ACCREDITO'
069000040506     O*         E            NACTES         1
069100040506     O*                                        +  1 'FATTURA NR.'
069200040506     O*                      FILFAT        Z   +  1
069300040506     O*                      NUMFAT        Z   +  1
069400040506     O*                                        +  1 'DEL'
069500040506     O*                      DATAF         Y   +  1
069600040506     O*                                        +  2 'DI '
069700040506     O*                      FATDIV            +  1
069800040506     O*                      FATIMP        2   +  1
069900040506     O*              10                          64 '    '
070000040506     O*         E    60      NACTES         2
070100040506     O*                                          50 '('
070200040506     O*                      WFATIM        2     64
070300040506     O*                                        +  1 ')'
070400040506     O*         E            NACTES      0  1
070500040506     O*                                        +  1 'CAUS'
070600040506     O*                                        +  1 'DESCRIZIONE'
070700040506     O*                                          70 'IMPORTO'
070800040506     O*                                          81 'ES.IVA'
070900040506     O*         E            NACTES         2
071000040506     O*                                        +  1 ' __ '
071100040506     O*                                        +  1 '________________________'
071200040506     O*                                        +  0 '________________________'
071300040506     O*                      DIVIMP              58
071400040506     O*                      IMPONI        2     74
071500040506     O*              11                          74 '    '
071600040506     O*                      TPALI             +  1
071700040506     O*                      CdIVA             +  1
071800040506     O*         E    61      NACTES         1
071900040506     O*                                          50 '('
072000040506     O*                      WIMPON        2     74
072100040506     O*                                        +  1 ')'
072200040506     O*         E            NAC1           1
072300040506     O*                                        + 28 'IMPONIBILE..............'
072400040506     O*                                        +  0 '..:'
072500040506     O*                      DIVIMP              58
072600040506     O*                      TOTIMP        2     74
072700040506     O*              12                          74 '    '
072800040506     O*         E            NAC1           1
072900040506     O*                                        + 28 'I.V.A...................'
073000040506     O*                                        +  0 '..:'
073100040506     O*                      DIVIMP              58
073200040506     O*                      IMPIVA        2     74
073300040506     O*              13                          74 '    '
073400040506     O*         E            NAC1           1
073500040506     O*                                        + 28 'IMPORTI ESENTI..........'
073600040506     O*                                        +  0 '..:'
073700040506     O*                      DIVIMP              58
073800040506     O*                      TOTESE        2     74
073900040506     O*              14                          74 '    '
074000040506     O*         E            NAC1           3
074100040506     O*                                        + 28 'TOTALE..................'
074200040506     O*                                        +  0 '..:'
074300040506     O*                      DIVIMP              58
074400040506     O*                      STORNO        2     74
074500040506     O*              15                          74 '    '
074600040506     O*         E            NACTE2         2
074700040506     O*                                        +  1 'SPEDIZIONE'
074800040506     O*                      ARBAAS            +  1
074900040506     O*                      ARBLNP            +  1
075000040506     O*                      ARBNRS            +  1
075100040506     O*                      ARBNSP        Z   +  1
075200040506     O*                                        +  1 ' / '
075300040506     O*                      ARBLNA            +  1
075400040506     O*                      DTBL              +  2
075500040506     O*         E            NACTE2         1
075600040506     O*                                        +  1 'MITT.'
075700040506     O*                      ARBRSM            +  1
075800040506     O*         E            NACTE2         1
075900040506     O*                      ARBINM            +  7
076000040506     O*                                          54 'RIF.MITT.'
076100040506     O*                      ARBRMN        Z   +  1
076200040506     O*         E            NACTE2         1
076300040506     O*                      ARBLOM            +  7
076400040506     O*                                          54 'COLLI    '
076500040506     O*                      ARBNCL        2   +  4
076600040506     O*         E            NACTE2         2
076700040506     O*                                        +  7 'CAP'
076800040506     O*                      ARBCAM            +  1
076900040506     O*                                        +  1 'PROV.'
077000040506     O*                      ARBPRM            +  1
077100040506     O*                                        +  1 'NAZ.'
077200040506     O*                      ARBNZM            +  1
077300040506     O*                                          54 'PESO     '
077400040506     O*                      ARBPKF        2   +  4
077500040506     O*                                        +  1 'VOLUME'
077600040506     O*                      ARBVLF        2   +  1
077700040506     O*         E            NACTE2      3  3
077800040506     O*                                        +  7 'NOTE: __________________'
077900040506     O*                                        +  0 '________________________'
078000040506     O*         E            NACTE2         3
078100040506     O*                                        +  7 '________________________'
078200040506     O*                                        +  0 '________________________'
078300040506     O*         E            NACSPE      1  2
078400040506     O*                                        +  7 'RECUPERATA CON SPEDIZIO'
078500040506     O*                                        +  0 'NE  NUMERO :'
078600040506     O*                                        +  1 '________________________'
078700040506     O*                                        +  0 '_____________'
078800040506     O*         E            NACSAL      1  1
078900040506     O*                                        + 30 'DISTINTI SALUTI'
079000040506     O*         E            NACSAL         3
079100040506     O*                      DLNA              + 30
079200040506     O*         E            NACFIR         2
079300040506     O*                                        +  6 'FIRMA DEL RESPONSABILE D'
079400040506     O*                                        +  0 'ELEGATO P.O. 1° LIVELLO'
079500040506     O*                                        +  3 '........................'
079600040506     O*         E            NACFIR         2
079700040506     O*                                        +  6 'FIRMA DEL RESPONSABILE D'
079800040506     O*                                        +  0 'ELEGATO P.O. 2° LIVELLO'
079900040506     O*                                        +  3 '........................'
080000040506     O*         E            NACFIR         2
080100040506     O*                                        +  6 'FIRMA DEL RESPONSBILE DI'
080200040506     O*                                        +  0 ' AREA                  '
080300040506     O*                                        +  3 '........................'
080400040506      *
080500950118**  CMDP
080600950118OVRPRTF FILE(QSYSPRT) SAVE(*YES) USRDTA(ELEN_NACCR)
080700920316** CMN
080800950118OVRPRTF FILE(LET132 ) OUTQ(*JOB      ) SAVE(*YES) USRDTA(RICH_NACCR)
