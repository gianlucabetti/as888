000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200030611      * TNTA04R *----------------------------------------------------*
000300030611      *        VISUALIZZAZIONE VARIAZIONI TARIFFA
000400940726      *--------------------------------------------------------------*
000500940726      *  Indicatori utilizzati:
000600940726      *  20   ON   SFLDSP
000700940726      *  21   ON   SFLDSPCTL
000800940726      *  21   OFF  SFLCLR
000900940726      *  23   ON   SFLEND
001000021212      *  30   Ciclo di lettura su file
001100021212      *  31   Generico
001200940726      *--------------------------------------------------------------*
001300030612     fTitav02l  if   e           k disk
001400030612     fTabel00f  if   e           k disk
001500030612     fCnAco00f  if   e           k disk
001600030612     fTnTam01l  if   e           k disk
001700030611     fTnta04d   cf   e             workstn sfile(TA04S01:NRR1)
001800030122
001900030612     d Tnta04ds      e ds
002000030612     d kpjba         e ds
002100030612     d Ds1P          e ds
002200030612     d DsSP          e ds
002300030612     d DsTR          e ds
002400030612     d Dsta01        e ds
002500030612     d ddatiute      e ds
002600030612     d azuteds       e ds                  extname(AZUTE00F)
002700030612     d tibs34ds      e ds                  inz
002800021212      *
002900021212      * Ds per la routine di controllo data  (8 CIFRE)
003000021212     d WLBDAT          DS                  INZ
003100021212     d  G02DAT                 1      8  0
003200021212     d  G02INV                 9     16  0
003300021212     d  G02ERR                17     17
003400021212     d  G02TGI                18     22  0
003500030612      *
003600030709     d$TarP_Cod        s              2    DIM(50)
003700030710     d$TarP_Des        s             20    DIM(50)
003800030612      *
003900030710     dTarTes           c                   CONST('TESTATA             ')
004000030710     dTarDet           c                   CONST('DETTAGLIO           ')
004100030710     dTarGia           c                   CONST('GIACENZA            ')
004200021212      *--------------------------------------------------------------*     *****
004300021212     c     *ENTRY        PLIST
004400030611     c                   PARM                    Tnta04Ds
004500030611      *
004600030612     c                   Exsr      Iniz
004700021212      *
004800021212      *   P U L I Z I A   S U B F I L E
004900021212      *
005000021212     c                   eval      *in20 = *off
005100021212     c                   eval      *in21 = *off
005200021212     c                   eval      *in23 = *off
005300021212      *
005400030611     c                   WRITE     TA04C01
005500021212      *
005600021212     c                   eval      *in21 = *on
005700021212     c                   eval      *in23 = *on
005800021212      *
005900021212     c                   Z-ADD     0             NRR1              4 0
006000021212      *
006100021212      * Riempio i campi del control
006200021212     c                   Exsr      RieCtl
006300021212      *
006400021212      *   C A R I C O    S U B F I L E
006500030121      *
006600030611      * Leggo il file dall'ultimo record
006700030612     C     KTav          SETLL     TITAV02L
006800030612     C     KTav          READE     TITAV02L                               30
006900021212      *
007000930525     C     *IN30         DOWEQ     '0'
007100021212      * Escludo i record annullati
007200030611     C                   IF        TavAtb = *blanks
007300021212      *
007400030711      * selezioni su tipo record/tariffe particolari
007500030711     C                   EXSR      SelTrd
007600030711     C                   IF        $SelOk = *on
007700021212      *
007800021212      * Riempio i campi del Sfl
007900021212     C                   EXSR      RieSfl
008000021212      *
008100940725     C                   ADD       1             NRR1
008200021212     C  n20              seton                                        20
008300030611     C                   WRITE     TA04S01
008400030711     C                   ENDIF
008500030711     C                   ENDIF
008600021212      *
008700021212      * Letture Sede/Filiale
008800030612     C     kTav          READE     TITAV02L                               30
008900021212     C                   ENDDO
009000021212      *
009100021212      *  E M E T T O    S U B F I L E
009200021212      *
009300021218     C     NRR1          IFNE      0
009400030722     C     *INKL         DOWEQ     *off
009500021212     C                   Z-ADD     1             NRR1
009600940725     C                   Z-ADD     1             REC1
009700030611     C                   WRITE     TA04T01
009800030611     C                   WRITE     TA04Z01
009900030611     C                   EXFMT     TA04C01
010000021212     C                   ENDDO
010100021212     C                   ENDIF
010200030122      * Subfile vuoto emetto una videata diversa
010300030122     c                   If        Nrr1 = *Zeros
010400030722     c                   Dow       Not *Inkl
010500030611     C                   WRITE     TA04T01
010600030611     C                   WRITE     TA04Z01
010700030710     C                   WRITE     TA04D01
010800030710     C                   EXFMT     TA04C01
010900030122     c                   EndDo
011000030122     c                   EndIf
011100021212      *
011200030122     C                   SETON                                        LR
011300021212      **************************************************************************
011400021212      *    Riempio i campi del control
011500021212      **************************************************************************
011600021212     C     RieCtl        BegSr
011700030611      *
011800030611     C                   clear                   V1CKsc
011900030611     C                   clear                   V1CKscD
012000030611     C                   clear                   V1CPrg
012100030611     C                   clear                   V1CCtr
012200030611     C                   clear                   V1CCtrD
012300030611     C                   clear                   V1CDcv
012400030611     C                   clear                   V1CDiv
012500021212      *
012600030613      * Codice cliente
012700030611     C                   z-add     I04Ksc        V1CKsc
012800030612      *
012900030613      * Ragione sociale cliente
013000030613     C                   IF        I04Rag = *blanks
013100030612     C     KAco          chain     CnAco000                           34
013200030612     C                   IF        *in34 = *off and Acoflg =' '
013300030612     C                   MOVEL     AcoRag        V1CKscD
013400030612     C                   ENDIF
013500030613     C                   ELSE
013600030613      * campo passato in DS
013700030613     C                   MOVEL     I04Rag        V1CKscD
013800030613     C                   ENDIF
013900030613     C
014000030613      *
014100030613      * Codice tariffa
014200030613     C                   z-add     I04Ctr        V1CCtr
014300030613      *
014400030613      * Decodifica codice tariffa
014500030613     C                   IF        I04DCT = *blanks
014600030613     C                   movel     V1CCtr        primo_byte        1
014700030613     C                   movel(p)  primo_byte    key
014800030613     C                   movel     'TR'          cod
014900030613     C     Ktab          chain     tabel                              34
015000030613     C                   IF        *in34 = *off and tblflg = ' '
015100030613     C                   movel     TblUni        DSTR
015200030613     C                   movel     §TrDes        V1CCtrD
015300030613     C                   ENDIF
015400030613     C                   ELSE
015500030613      * campo passato in DS
015600030613     C                   MOVEL     I04DCT        V1CCtrD
015700030613     C                   ENDIF
015800030612      *
015900030613      * Descrizione e divisa della tariffa
016000030612     C     Ktav          chain     Tntam01l                           34
016100030612     C                   IF        *in34 = *off and TamAtb =' '
016200030613     C                   IF        I04Dcv = *blanks
016300030612     C                   movel     TamDcv        V1CDcv
016400030613     C                   ELSE
016500030613      * campo passato in DS
016600030613     C                   movel     I04Dcv        V1CDcv
016700030613     C                   ENDIF
016800030613      *
016900030613     C                   IF        I04Div = *blanks
017000030612     C                   movel     TamFlo        DsTa01
017100030612     C                   movel     §Tadiv        V1CDiv
017200030613     C                   ELSE
017300030613      * campo passato in DS
017400030613     C                   movel     I04Div        V1CDiv
017500030613     C                   ENDIF
017600030612     C                   ENDIF
017700030613      *
017800030613      * Progressivo tariffa
017900030613     C                   z-add     I04Prg        V1CPrg
018000030612      *
018100021212     C                   EndSr
018200030711      **************************************************************************
018300030711      *    Selezioni su tipo record /Tariffe particolari
018400030711      **************************************************************************
018500030711     c     SelTrD        BegSr
018600030711     c                   move      *on           $SelOk            1
018700030711     c                   SELECT
018800030711      *
018900030711      * Ho richiesto un tipo record , ma non una tariffa particolare
019000030711     c                   WHEN      I04Trd <> *blanks and I04Trd <> 'PAR'
019100030711     c                             and TavTrd <> I04Trd
019200030711     c                   move      *off          $selOk
019300030711      *
019400030711      * Ho richiesto tutte le tariffe particolari
019500030711     c                   WHEN      I04Trd ='PAR' and I04Ftc = *blanks
019600030711     c                             and TavTrd <> 'PAR'
019700030711     c                   move      *off          $selOk
019800030711      *
019900030711      * Ho richiesto una una tariffa particolare
020000030711     c                   WHEN      I04Trd ='PAR' and I04Ftc <> *blanks
020100030711     c                             and TavFtc <> I04Ftc
020200030711     c                   move      *off          $selOk
020300030711     c                   ENDSL
020400030711      *
020500030711     c                   EndSr
020600021212      **************************************************************************
020700021212      *    Riempio i campi del subfile
020800021212      **************************************************************************
020900021212     c     RieSfl        BegSr
021000030127
021100030612     c                   Clear                   V1sTtr
021200030612     c                   Clear                   V1sCta
021300030612     c                   Clear                   V1sDav
021400030612     c                   Clear                   V1sOrv
021500030611     c                   Clear                   V1sPru
021600030611     c                   Clear                   V1sNoj
021700030612      * Tipo tariffa:se non è 'particolare' decodifico il tipo tariffa
021800030612      * TavTrd, altrimenti ricerco la decodifica della tariffa particolare
021900030612     c                   IF        TavTrd <>'PAR'
022000030612      *
022100030612     c                   select
022200030612     c                   when      tavTrd = 'TES'
022300030710     c                   movel(p)  TarTes        v1sTtr
022400030612     c                   when      tavTrd = 'DET'
022500030710     c                   movel(p)  TarDet        v1sTtr
022600030612     c                   when      tavTrd = 'GIA'
022700030710     c                   movel(p)  TarGia        v1sTtr
022800030612     c                   endsl
022900030612     c                   ELSE
023000030612      * recupero la descrizione della tariffa particolare
023100030612      * dalla schiera caricata
023200030612     c                   z-add     1             xx
023300030612     c     TavFtc        lookup    $TarP_Cod(XX)                          31
023400030711     c   31              do
023500030711     c                   move      *blanks       campo3            3
023600030711     c                   movel     $TarP_Cod(XX) campo3
023700030711     c                   move      '-'           campo3
023800030711     c                   movel     campo3        v1STtr
023900030711     c                   move      $TarP_Des(XX) v1STtr
024000030711     c                   enddo
024100030612     c                   ENDIF
024200030612      * Tipo azione
024300030611     c                   SELECT
024400030612     c                   WHEN      TavCta = 'A'
024500030612     c                   movel     'Ann.'        V1sCta
024600030612     c                   WHEN      TavCta = 'I'
024700030612     c                   movel     'Ins.'        V1sCta
024800030612     c                   WHEN      TavCta = 'M'
024900030612     c                   movel     'Man.'        V1sCta
025000030612     c                   ENDSL
025100030612      *
025200030612     c* decodifica codice tariffa particolare    V1sFtc
025300030611      * data variazione
025400030611     c                   clear                   WLBDAT
025500030611     C                   Z-ADD     TavDav        G02INV
025600030611     C                   Z-ADD     *ZEROS        G02DAT
025700030611     C                   MOVEL     3             G02ERR
025800030611     C                   CALL      'XSRDA8'
025900030611     C                   PARM                    WLBDAT
026000030611     C                   Z-ADD     G02DAT        V1SDav
026100030611      *
026200030611     c                   z-add     TavOrv        V1sOrv
026300030611     c                   movel     TavPru        V1sPru
026400030611     c                   movel     TavNoj        V1sNoj
026500021212      *
026600021212     c                   EndSr
026700021212      **************************************************************************
026800021212      *    R O U T I N E     I N I Z I A L E
026900021212      **************************************************************************
027000030612     C     INIZ          BEGSR
027100021212      *
027200030612     c     *dtaara       define    §azute        azuteds
027300030612     c     *dtaara       define    §datiute      ddatiute
027400030612
027500030612     C                   in(E)     *dtaara
027600030612     C                   IF        %Error  or  RSUT = *blanks
027700030612     c                   clear                   tibs34ds
027800030612     C                   call      'TIBS34R'
027900030612     C                   parm                    Tibs34Ds
028000030612     C                   in        *dtaara
028100030612     c                   ENDIF
028200030612      *
028300030612      * Carico in schiera codici e descrizione delle tariffe
028400030612      * particolari da tabella 1P
028500030612     C                   z-add     1             codut             1 0
028600030612     C                   movel     '1P'          cod               2
028700030612      *
028800030612     C                   z-add     1             XX                2 0
028900030612     C     KTab2         setll     tabel
029000030612     C     KTab2         reade     tabel                                  30
029100030612    1C                   DOW       *in30 ='0'
029200030612      *
029300030612      * Controllo validità tariffa particolare per la gestione tariffe
029400030612    2C                   IF        TblFlg = *blanks
029500030612     C                   movel     tbluni        Ds1P
029600030612      *
029700030612      * Flag di comodo per tabella 'SP'
029800030612     C                   clear                   FlagSP
029900030612    3C                   IF        §1pUta = 'S'
030000030612      *
030100030612     C                   movel     TblKey        $TarP_Cod(XX)
030200030612     C                   movel     §1PDes        $TarP_Des(XX)
030300030612      *
030400030612      * Per tariffa FedEx controllo se devo recupare la descrizione
030500030612      * dalla tabella SP
030600030612     C                   IF        I04NDF = 'F' and §1pDFE ='S'
030700030612     C                   movel     '1'           FlagSP            1
030800030612     C                   clear                   dsSP
030900030612     C                   movel     'SP'          Cod
031000030612     C                   movel     *blanks       Key
031100030612     C                   movel     $TarP_Cod(XX) Key               8
031200030612      *
031300030612     C     Ktab          chain     tabel                              34
031400030612     C                   IF        *in34 = *off and tblflg = ' '
031500030612     C                   movel     TblUni        DsSP
031600030612     C                   ENDIF
031700030612     C                   IF        §SPDFE <>*BLANKS
031800030612     C                   movel     §SPDFE        $TarP_Des(XX)
031900030612     C                   ENDIF
032000030612     C                   ENDIF
032100030612     C                   add       1             XX
032200030612      *
032300030612     C                   IF        FlagSP <>*BLANKS
032400030612     C                   movel     '1P'          Cod
032500030612     C     Ktab          setgt     Tabel
032600030612     C                   ENDIF
032700030612     C                   ENDIF
032800030612     C                   ENDIF
032900030612     C     Ktab2         reade     Tabel                                  30
033000030612    1C                   ENDDO
033100030612     c
033200030612      *
033300030612     C     KTav          KLIST
033400030611     C                   KFLD                    I04Ksc
033500030611     C                   KFLD                    I04Ctr
033600030611     C                   KFLD                    I04Prg
033700021212      *
033800030612     C     Ktab2         KLIST
033900030612     C                   KFLD                    CODUT
034000030612     C                   KFLD                    COD
034100030612      *
034200030612     C     Ktab          KLIST
034300030612     C                   KFLD                    CODUT
034400030612     C                   KFLD                    COD
034500030612     C                   KFLD                    KEY
034600030612      *
034700030612     C     KAco          KLIST
034800030612     C                   KFLD                    CODUT
034900030612     C                   KFLD                    DUTKCI
035000030612     C                   KFLD                    I04Ksc
035100940726     C                   ENDSR
