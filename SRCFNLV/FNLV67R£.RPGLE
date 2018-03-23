000100980223     H DECEDIT('0,') DATEDIT(*DMY.)
000200010320
000300980223     FAZORG01L  IF   E           K DISK
000400020618     F*
000500020618     FFLTR351L  UF A E           K DISK
000600020618     F*
000700020618     FFNLV67D   CF   E             WORKSTN
000800020618     F                                     SFILE(LV67S02:NRR)
000900010320
001000040924     D TES             S             35    DIM(6)  CTDATA PERRCD(1)
001100080526     D MSG             S             78    DIM(21) CTDATA PERRCD(1)
001200011210     D*
001300011210     D POX             S              1    DIM(999) INZ(*ALL'0')
001400980223     D*
001500980223     D KPJBA         E DS
001600050421      * - Parametri x Controllo profilo utenti
001700050421     d TIBS34ds      e ds
001800050421      * - Ds di riferimento al file esterno AZUTE00F
001900050421     d AZUTEds       e ds                  extname(AZUTE00F)
002000080526     d dUte01        e ds
002100050421      * - Ds per dati organigramma
002200050421     d dDATIUTE      e ds
002300050421      *
002400010320     D dtr3des       e ds
002500020618     D FNLV50DS      E DS
002600020618     D* DS per TRUL06R - caricamento £X
002700020618     D TRUL06DS      E DS
002800020618     D  LIN                    1     90  0    DIM(30)
002900020618     D*
003000020618     D PARAM1          DS                  INZ
003100020618     D  PA1CTL                 1      1
003200020618     D  PA1DPL                 2      3
003300020618     D  PA1APL                 4      5
003400020618     D  PA1POG                 6      8
003500050421      *
003600050421     d DsTNSD          ds                  inz
003700050421     d  §cod                          3    inz
003800050421     d  §tip                          1    inz
003900050421     d  §des                         25    inz
004000050421     d  §pra                          3    inz
004100011210     D*
004200011210     D XX              S              3  0 INZ
004300011210     D wXXa            S              3    INZ(*zeros)
004400011210     D wDes            S             30    INZ
004500020618     D wCTL            S                   LIKE(TR3CTL) INZ
004600020618     D wPOG            S                   LIKE(TR3pog) INZ
004700020618     D WRKPOG          S              3  0 INZ
004800020618     D WRKPRB          S              3  0 INZ
004900020618     D NRR             S              4  0 INZ
005000020618     D $FLTR3xxx       S              1    inz(*off)
005100050421     d Wcampo          s             10    inz
005200050421     d Wpo             s              3  0 inz
005300050421     d WpoD            s             20    inz
005400050421     d Wzona3          s              3  0 inz
005500050422     d WzonaA          s              2    inz
005600050420     d wapice          s              1    inz('''')
005700050420     d wpos            s              3  0 inz
005800011211     D*
005900011211     D £POX            C                   CONST(995)
006000020724     D* nr.di righe del sfl (SFLPAG)
006100020724     D SFLPAG          C                   CONST(16)
006200050421      * Nr. di record nel sfl (SFLSIZ)
006300070109     ***d C_SflSiz        c                   const(900)
006400110414     ***d C_SflSiz        c                   const(1500)
006500110414     d c_SflSiz        c                   const(2200)
006600020724     D* caratteri numerici consentiti
006700020724     D DigitN          C                   CONST('1234567890')
006800010320
006900020618      *****************************************************************
007000020618      * RIEPILOGO INDICATORI
007100020618      *****************************************************************
007200020618      * 05    - SPROTEGGO P.O. IN GESTIONE
007300020618      * 06    - NON ABILITO TASTO FUNZIONALE F18=Cambio P.O.
007400020618      * 20    - SFLDSP
007500020618      * 21    - SFLDSPCTL
007600020618      * 24    - SFLINZ
007700020618      * 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
007800020618      * 31/33 - DI COMODO
007900020618      * 40/48 - ERRORI
008000020618      *****************************************************************
008100050426      *
008200020618      *---------------------------------------------------------------*
008300050426      *
008400020618      * DEFINIZIONE CHIAVI
008500020618      *
008600020618     C     K03TR350      KLIST
008700020618     C                   KFLD                    wPOG
008800020618     C                   KFLD                    V1CCTL
008900020618     C                   KFLD                    V1CNPL
009000020618      *
009100020618     C     K08TR350      KLIST
009200020618     C                   KFLD                    TR3POG
009300020618     C                   KFLD                    TR3CTL
009400020618     C                   KFLD                    TR3NPL
009500020618     C                   KFLD                    TR3LNP
009600020618     C                   KFLD                    TR3LNA
009700020618     C                   KFLD                    TR3ZON
009800020618     C                   KFLD                    TR3PRB
009900020618     C                   KFLD                    TR3BAI
010000020618      *
010100020618     C     K06TR350      KLIST
010200020618     C                   KFLD                    TR3POG
010300020618     C                   KFLD                    TR3CTL
010400020618     C                   KFLD                    TR3NPL
010500020618     C                   KFLD                    TR3LNP
010600020618     C                   KFLD                    TR3LNA
010700020618     C                   KFLD                    TR3ZON
010800050426      *
010900020618      *---------------------------------------------------------------*
011000050426      *
011100980223     C     *ENTRY        PLIST
011200980223     C                   PARM                    KPJBA
011300050426      *
011400050421      * Reperimento dati aziendali
011500050421     c     *dtaara       define    §azute        AZUTEds
011600050421     c     *dtaara       define    §datiute      dDATIUTE
011700050421     c                   in(E)     *dtaara
011800050421     c                   if        %ERROR or RSUT = *blanks
011900050421     c                   clear                   Tibs34Ds
012000050421     c                   call      'TIBS34R'
012100050421     c                   parm                    Tibs34Ds
012200050421     c                   in        *dtaara
012300050421     c                   endif
012400050421     c                   movel     DUTdpo        VTCrsu
012500080526     c                   movel     UTEfaf        dUte01
012600050426      *
012700980223      * TESTATA PGM
012800980224     C                   MOVEL     TES(1)        VTCTES
012900020618      *
013000020618      * Imposto il "P.O. IN GESTIONE"
013100050421     c                   move      dutPOU        V1Cpog
013200050421if  1c                   IF        dutLPO = '2' or dutLPO = *blanks
013300020618     c                   eval      *in06  = *on
013400020618e   1c                   ENDIF
013500020618     c                   move      V1Cpog        WRKPOG
013600020618     c                   exsr      CTRFGS
013700020618     c                   movel     ORGdes        V1Dpog
013800020618     c                   SETOFF                                       9028
013900080526
014000080526      * Se utente NON abilitato: visualizza messaggio per uscire
014100080526      *   subito dal programma
014200080526     c                   if        §UTEmps <> 'S'
014300080526     c                   eval      *in28   =  *on
014400080526     c                   eval      V1CMsg  =  Msg(21)
014500080526     c                   endif
014600050426      *
014700020618      *---------------------------------------------------------------*
014800050426      *
014900980223     C     INIZIO        TAG
015000980223     C                   CLEAR                   V1CCTL
015100980223     C                   CLEAR                   V1CNPL
015200980223      *
015300980223     C     FOR01         TAG
015400980223     C                   WRITE     LV67T01
015500980223     C                   EXFMT     LV67D01
015600980223      *
015700980223      * pulizia campo messaggio e relativo indicatore  (*IN28)
015800980224     C                   CLEAR                   V1CMSG
015900980223      * azzero indicatori relativi ai messaggi di errore
016000050426     C                   SETOFF                                       28  90
016100050426     C                   SETOFF                                       4048
016200080526      *
016300080526      * Utente non abilitato
016400080624     c     §UTEmps       cabne     'S'           Fine
016500980223      *
016600050426      * F3=Fine lavoro
016700980223     C   KC              GOTO      FINE
016800020618      *
016900020618      * F18=Cambio P.O. in gestione
017000020618     c   KS              eval      *in05 = *on
017100020618     c   KS              goto      FOR01
017200020618      *
017300020618      * 05 on --> controllo P.O. in gestione
017400020618     c                   if        *in05
017500020618     C                   clear                   V1Dpog
017600020618     c                   move      V1Cpog        WRKPOG
017700020618     c                   exsr      CTRFGS
017800020618     c   90              movel     D50MSG        V1CMSG
017900020618     c   90              goto      FOR01
018000020618     c                   movel     ORGdes        V1Dpog
018100020618     c                   eval      *in05 = *off
018200020618     c                   endif
018300980223      *
018400050426      * Controllo dati del formato D01
018500050426     c                   exsr      CtrD01
018600050426     c   28              goto      FOR01
018700050426      *
018800020618      *---------------------------------------------------------------*
018900050426      *
019000980223      * GESTIONE VIDEATA MANUTENZIONE
019100980223     C     FOR02         TAG
019200980223     C                   EXSR      CARSF1
019300980223     C                   EXSR      GESSF1
019400980224      *
019500980224     C                   GOTO      INIZIO
019600050426      *
019700020618      *---------------------------------------------------------------*
019800050426      *
019900980224     C     FINE          TAG
020000020618     C                   if        $FLTR3xxx = *on
020100020618     C                   clear                   KPJBU
020200020618     C                   call      'FNLV67R2'
020300020618     C                   parm                    KPJBA
020400020618     C                   endif
020500980224     C                   SETON                                        LR
020600020618      **********************************************************************
020700020618      * CONTROLLO IL P.O. IN IN GESTIONE
020800020618      **********************************************************************
020900020618     C     CTRFGS        BEGSR
021000020618      *
021100020618     C                   setoff                                       9028
021200020618     C                   clear                   FNLV50DS
021300020618     C                   movel     KNMUS         D50PRU
021400020618     C                   move      WRKPOG        D50FGS
021500020618     C                   call      'FNLV50R'
021600020618     C                   parm                    FNLV50DS
021700020618      *
021800020618if  1C                   IF        D50ERR <> *blanks
021900020618      *
022000020618     C                   seton                                        9028
022100020618      *
022200020618x   1C                   else
022300020618      *
022400020618      * decodifica
022500020618     C     WRKPOG        chain     AZORG
022600020618if  2C                   IF        not %found(AZORG01L)
022700020618     C                   clear                   orgDES
022800020618e   2C                   ENDIF
022900020618      *
023000020618e   1C                   ENDIF
023100020618      *
023200020618     C                   ENDSR
023300050426
023400050426      *---------------------------------------------------------------*
023500050426      * CTRD01 * CONTROLLO DATI IN FORMATO D01                        *
023600050426      *---------------------------------------------------------------*
023700050426     c     CtrD01        BEGSR
023800050426      *
023900050426      * Tipo lavorazione
024000050426      * - effettiva immissione
024100050426     c                   if           V1Cctl = *blanks
024200050426     c                             or V1Cctl = *zeros
024300050426     c                   movel     MSG(12)       V1Cmsg
024400050426     c                   seton                                        48  28
024500050426     c                   goto      EndCtrD01
024600050426     c                   endif
024700050426      * - controllo effettiva numericità del campo
024800050426     c     DigitN        check     V1Cctl                                 28
024900050426     c                   if        *in28 = *on
025000050426     c                   seton                                        48
025100050426     c                   movel     MSG(14)       V1Cmsg
025200050426     c                   goto      EndCtrD01
025300050426     c                   endif
025400050426      *
025500050426      * Piano lavorazione
025600050426      * - il primo carattere del piano di lavorazione deve essere uguale
025700050426      *   al tipo lavorazione
025800050426     c                   movel     V1Cnpl        wCTL
025900050426     c                   if        wCTL <> V1Cctl
026000050426     c                   movel     MSG(1)        V1Cmsg
026100050426     c                   seton                                        40  28
026200050426     c                   goto      EndCtrD01
026300050426     c                   endif
026400050426      * - controllo effettiva numericità del campo
026500050426     c     DigitN        check     V1Cnpl                                 28
026600050426     c                   if        *in28 = *on
026700050426     c                   seton                                        40
026800050426     c                   movel     MSG(14)       V1Cmsg
026900050426     c                   goto      EndCtrD01
027000050426     c                   endif
027100050426      *
027200050426     c     EndCtrD01     ENDSR
027300980223      *-------------------------------------------------------------------------
027400980223      * CARICAMENTO SUBFILE
027500980223      *-------------------------------------------------------------------------
027600980223     C     CARSF1        BEGSR
027700980223      * Pulizia ed inizializzazione subfile
027800980224     C                   SETOFF                                       2021
027900980223     C                   SETON                                        24
028000980223     C                   WRITE     LV67C02
028100980223     C                   SETOFF                                       24
028200020618     C                   Z-ADD     *ZEROS        NRR
028300020724     C                   clear                   V2CPSZ
028400980223      * Caricamento subfile
028500020618     C                   move      V1Cpog        wPOG
028600020618     C     K03TR350      SETLL     FLTR3500
028700020618     C     K03TR350      READE     FLTR3500                               31
028800980223      * se non esiste alcun record scrivo nel subfile i record di mancata
028900000221      * lettura e zona non prevista e altri 2
029000980223    1C     *IN31         IFEQ      *ON
029100980223     C                   ADD       1             NRR
029200980224     C     NRR           CHAIN     LV67S02                            32
029300980223    2C  N32              DO
029400980227     C                   MOVE      '999'         V2SLNP
029500980227     C                   MOVE      '999'         V2SLNA
029600980223     C                   Z-ADD     99            V2CZON
029700980223     C                   Z-ADD     *ZEROS        V2CBAI
029800980223     C                   MOVE      *BLANKS       V2DLNP
029900980227     C                   MOVEL     'TUTTE'       V2DLNP
030000980223     C                   MOVE      *BLANKS       V2DLNA
030100980227     C                   MOVEL     'TUTTE'       V2DLNA
030200980223     C                   MOVE      *BLANKS       V2CATB
030300980224     C                   UPDATE    LV67S02
030400980223    2C                   END
030500980223     C*
030600980223     C                   ADD       1             NRR
030700980224     C     NRR           CHAIN     LV67S02                            32
030800980223    2C  N32              DO
030900980227     C                   MOVE      '998'         V2SLNP
031000980227     C                   MOVE      '998'         V2SLNA
031100980223     C                   Z-ADD     99            V2CZON
031200980223     C                   Z-ADD     *ZEROS        V2CBAI
031300980223     C                   MOVE      *BLANKS       V2DLNP
031400980227     C                   MOVEL     TES(2)        V2DLNP
031500980223     C                   MOVE      *BLANKS       V2DLNA
031600980227     C                   MOVEL     TES(2)        V2DLNA
031700980223     C                   MOVE      *BLANKS       V2CATB
031800980224     C                   UPDATE    LV67S02
031900000221    2C                   END
032000000221     C*
032100000221     C                   ADD       1             NRR
032200000221     C     NRR           CHAIN     LV67S02                            32
032300000221    2C  N32              DO
032400000221     C                   MOVE      '997'         V2SLNP
032500000221     C                   MOVE      '997'         V2SLNA
032600000221     C                   Z-ADD     99            V2CZON
032700000221     C                   Z-ADD     *ZEROS        V2CBAI
032800000221     C                   MOVE      *BLANKS       V2DLNP
032900000221     C                   MOVEL     TES(3)        V2DLNP
033000000221     C                   MOVE      *BLANKS       V2DLNA
033100000221     C                   MOVEL     TES(3)        V2DLNA
033200000221     C                   MOVE      *BLANKS       V2CATB
033300000221     C                   UPDATE    LV67S02
033400000221    2C                   END
033500000221     C                   ADD       1             NRR
033600000221     C     NRR           CHAIN     LV67S02                            32
033700000221    2C  N32              DO
033800000221     C                   MOVE      '996'         V2SLNP
033900000221     C                   MOVE      '996'         V2SLNA
034000000221     C                   Z-ADD     99            V2CZON
034100000221     C                   Z-ADD     *ZEROS        V2CBAI
034200000221     C                   MOVE      *BLANKS       V2DLNP
034300000221     C                   MOVEL     TES(4)        V2DLNP
034400000221     C                   MOVE      *BLANKS       V2DLNA
034500000221     C                   MOVEL     TES(4)        V2DLNA
034600000221     C                   MOVE      *BLANKS       V2CATB
034700000221     C                   UPDATE    LV67S02
034800000221    2C                   END
034900011210     C                   ADD       1             NRR
035000011210     C     NRR           CHAIN     LV67S02                            32
035100011210    2C  N32              DO
035200011210     C                   MOVE      '995'         V2SLNP
035300011210     C                   MOVE      '995'         V2SLNA
035400011210     C                   Z-ADD     99            V2CZON
035500011210     C                   Z-ADD     *ZEROS        V2CBAI
035600011210     C                   MOVE      *BLANKS       V2DLNP
035700011210     C                   MOVEL     TES(5)        V2DLNP
035800011210     C                   MOVE      *BLANKS       V2DLNA
035900011210     C                   MOVEL     TES(5)        V2DLNA
036000011210     C                   MOVE      *BLANKS       V2CATB
036100011210     C                   UPDATE    LV67S02
036200011210    2C                   END
036300980223      *
036400980223   X1C                   ELSE
036500980223      **** Se il tipo di lavorazione ed il piano esistono carico sul subfile
036600980223      **** in manutenzione
036700980223    2C     *IN31         DOWEQ     *OFF
036800980224     C                   ADD       1             NRR
036900980224     C     NRR           CHAIN     LV67S02                            32
037000980227     C                   MOVE      TR3LNP        V2SLNP
037100980227     C                   MOVE      TR3LNP        V2CLNP
037200980227     C                   MOVE      TR3LNA        V2SLNA
037300980227     C                   MOVE      TR3LNA        V2CLNA
037400980223     C                   MOVE      TR3ZON        V2CZON
037500980223     C                   MOVE      TR3BAI        V2CBAI
037600010320     C                   MOVEl     TR3DES        dtr3des
037700010320     C                   eval      v2spst = §tr3pista
037800010320     c                   eval      v2sopt = §tr3operat
037900010320     c                   eval      v2splt = §tr3pallet
038000010320     c                   movel     §tr3descri    v2ddes
038100980223      **** Cerco decodifica linea di partenza e linea di arrivo
038200050421     c                   move      V2Clnp        Wpo
038300050421     c                   exsr      sr_CtrPO
038400050421     c                   movel     WpoD          V2Dlnp
038500050421     c                   move      V2Clna        Wpo
038600050421     c                   exsr      sr_CtrPO
038700050421     c                   movel     WpoD          V2Dlna
038800040924     C     V2CLNA        IFEQ      990
038900040924     C                   MOVEL     *BLANKS       V2DLNA
039000040924     C                   MOVEL     TES(6)        V2DLNA
039100040924     C                   END
039200980223     C                   MOVE      *BLANKS       V2CATB
039300980223      * scrivo il subfile
039400980224     C   32              WRITE     LV67S02
039500980224     C  N32              UPDATE    LV67S02
039600980223      *
039700020618     C     K03TR350      READE     FLTR3500                               31
039800980223    2C                   ENDDO
039900050426    1C                   ENDIF
040000980223     C                   ENDSR
040100980223      *-------------------------------------------------------------------------
040200980223      * GESTIONE SUBFILE
040300980223      *-------------------------------------------------------------------------
040400980223     C     GESSF1        BEGSR
040500980223      *
040600980311     C                   WRITE     LV67Z02
040700980223     C                   Z-ADD     1             REC
040800980223     C                   SETON                                        2021
040900980223      *
041000980224    1C                   DO        *HIVAL
041100020724     C                   EXSR      POSCUR
041200980224     C                   EXFMT     LV67C02
041300980224      * pulizia campo messaggio e relativo indicatore  (*IN28)
041400980223     C                   CLEAR                   VCCMSG
041500050421     C                   SETOFF                                       28  90
041600980224      * azzero indicatori relativi ai messaggi di errore
041700980224     C                   SETOFF                                       414243
041800050420     C                   SETOFF                                       4447
041900050421      * F12=Ritorno
042000050421if  2c                   if        *inKL = *on
042100050421     c                   leave
042200050421e   2c                   endif
042300050421      * gestione videata di inserim. dati    F8
042400050421if  2c                   if        *inKH = *on
042500050421     c                   exsr      sr_InsAutom
042600050421     c                   iter
042700050421e   2c                   endif
042800980224      * gestione videata della duplicazione  F13
042900980224    2C     *INKM         IFEQ      *ON
043000980224     C                   EXSR      DUPTR
043100980311     C                   WRITE     LV67Z02
043200980224     C                   ITER
043300980224    2C                   ENDIF
043400980224      * richiamo programma di stampa         F10
043500980224    2C     *INKJ         IFEQ      *ON
043600020618     C                   MOVE      V1CPOG        PA1POG
043700980305     C                   MOVE      V1CCTL        PA1CTL
043800980305     C                   MOVE      V1CNPL        PA1DPL
043900980305     C                   MOVE      V1CNPL        PA1APL
044000980305     C                   MOVEL     PARAM1        KPJBU
044100980305     C                   MOVEL     'LV69'        KCOAZ
044200980305     C                   CALL      'BCH10'
044300980305     C                   PARM                    KPJBA
044400980305      *
044500980305     C                   ITER
044600980224    2C                   ENDIF
044700980223      * controllo dati inseriti a video      Enter
044800980224     C                   EXSR      CTRSF1
044900980224      * errori
045000980224     C   28              ITER
045100980224      * gestione conferma dati               F06
045200980224    2C     *INKF         IFEQ      *ON
045300980224     C                   EXSR      CONSF1
045400980224     C                   LEAVE
045500980224    2C                   ENDIF
045600980224      *
045700980224    1C                   ENDDO
045800980224      *
045900980224     C                   ENDSR
046000980224      *-------------------------------------------------------------------------
046100980224      * CONTROLLO DATI SUBFILE
046200980224      *-------------------------------------------------------------------------
046300980224     C     CTRSF1        BEGSR
046400980223     C                   Z-ADD     1             NRR
046500011210     C                   RESET                   POX
046600980223      *
046700980224    1C                   DO        *HIVAL
046800980224      * Spengo gli indicatori di errore
046900050421     C                   SETOFF                                       28  90
047000050421     C                   SETOFF                                       414243
047100050421     C                   SETOFF                                       444750
047200980224      *
047300980224     C     NRR           CHAIN     LV67S02                            32
047400980224     C   32              LEAVE
047500020618      *
047600020618     C                   if        V2Catb = 'A'
047700020618     C                   clear                   V2Dlnp
047800020618     C                   clear                   V2Dlna
047900020618     C                   goto      GIUCTR
048000020618     C                   endif
048100980224      * controllo se inserito almeno un campo della riga e se valido
048200980227     C     V2SLNP        IFNE      *ZEROS
048300980227     C     V2SLNP        ANDNE     *BLANKS
048400980227     C                   SETON                                        50
048500980227     C                   END
048600980227     C  N50V2SLNA        IFNE      *ZEROS
048700980227     C     V2SLNA        ANDNE     *BLANKS
048800980227     C                   SETON                                        50
048900980227     C                   END
049000980224     C  N50V2CZON        COMP      *ZEROS                             5050
049100980224     C  N50V2CBAI        COMP      *ZEROS                             5050
049200020724     C                   clear                   V2Dlnp
049300020724     C                   clear                   V2Dlna
049400980224      * almeno un campo e' stato inserito
049500980224    2C     *IN50         IFEQ      *ON
049600980224      * Controllo linea di partenza
049700980227    3C     V2SLNP        IFNE      *ZEROS
049800980227     C     V2SLNP        ANDNE     *BLANKS
049900980227     C*
050000980227     C     '?'           SCAN      V2SLNP                                 30
050100980227     C*
050200980227     C     *IN30         IFEQ      *ON
050300050421     c                   eval      Wcampo =  'V2SLNP    '
050400050421     c                   exsr      sr_Search
050500050421     C                   SETON                                        41
050600050421     C                   GOTO      GIUCTR
050700050421     C                   ENDIF
050800980224    3C                   END
050900050421      *
051000050421     C                   MOVE      V2SLNP        V2CLNP            3 0
051100050421     c                   move      V2Slnp        Wpo
051200050421     c                   exsr      sr_CtrPO
051300050421     c                   movel     WpoD          V2Dlnp
051400050421      * *in90 => P.O. partenza inesistente
051500050421if  3c                   if        *in90
051600050422     c                   eval      V2Cpsz =  NRR
051700050421     c                   movel     Msg(3)        VCCmsg
051800050421     c                   seton                                        4128
051900050421     c                   clear                   V2Dlnp
052000050421     c                   goto      GIUCTR
052100050421e   3c                   endif
052200980224     C*
052300980227      * Controllo linea di arrivo
052400980227    3C     V2SLNA        IFNE      *ZEROS
052500980227     C     V2SLNA        ANDNE     *BLANKS
052600980227     C*
052700980227     C     '?'           SCAN      V2SLNA                                 30
052800980227     C*
052900980227     C     *IN30         IFEQ      *ON
053000050421     c                   eval      Wcampo =  'V2SLNA    '
053100050421     c                   exsr      sr_Search
053200980227     C                   SETON                                        42
053300980227     C                   GOTO      GIUCTR
053400980227     C                   ENDIF
053500980227    3C                   END
053600050421      *
053700050421     C                   MOVE      V2SLNA        V2CLNA            3 0
053800050421     c                   move      V2Slna        Wpo
053900050421     c                   exsr      sr_CtrPO
054000050421     c                   movel     WpoD          V2Dlna
054100040924     C     V2CLNA        IFEQ      990
054200040924     C                   MOVEL     *BLANKS       V2DLNA
054300040924     C                   MOVEL     TES(6)        V2DLNA
054400050421     c                   else
054500050421      * *in90 => P.O. arrivo inesistente
054600050421if  4c                   if        *in90
054700050422     c                   eval      V2Cpsz =  NRR
054800050421     c                   movel     Msg(5)        VCCmsg
054900050421     c                   seton                                        4228
055000050421     c                   clear                   V2Dlna
055100050421     c                   goto      GIUCTR
055200050421e   4c                   endif
055300040924     C                   END
055400980227     C*
055500980224      * Controllo linea di arrivo
055600980224      * Controllo Zona ..... deve essere <= 99
055700980224    3C     V2CZON        IFGT      99
055800050422     c                   eval      V2Cpsz =  NRR
055900980224     C                   MOVEL     MSG(6)        VCCMSG
056000980224     C                   SETON                                        4328
056100980224     C                   GOTO      GIUCTR
056200980224    3C                   END
056300990129      * Controllo Baia ..... deve essere > 0 <= 45
056400010613      * dal 13/06/01 il controllo va spostato a 60 (Bocchi)
056500980224    3C     V2CBAI        IFEQ      *ZEROS
056600050422     c                   eval      V2Cpsz =  NRR
056700980224     C                   MOVEL     MSG(7)        VCCMSG
056800980224     C                   SETON                                        4428
056900980224     C                   GOTO      GIUCTR
057000980224    3C                   END
057100980224      *
057200010613    3C     V2CBAI        IFGT      60
057300050422     c                   eval      V2Cpsz =  NRR
057400980224     C                   MOVEL     MSG(8)        VCCMSG
057500980224     C                   SETON                                        4428
057600980224     C                   GOTO      GIUCTR
057700980224    3C                   END
057800050420      * controllo che nella descrizione non sia stato immesso il carattere apice
057900050420      * questo carattere non va bene x il pc
058000050420     c                   eval      wpos = %scan(wapice:v2ddes)
058100050420     c                   if        wpos > 0
058200050422     c                   eval      V2Cpsz =  NRR
058300050420     c                   eval      vccmsg = msg(15)
058400050420     c                   eval      *in28 = *on
058500050420     c                   eval      *in47 = *on
058600050420     c                   goto      giuctr
058700050420     c                   endif
058800011210      *
058900011210      * memorizzo i P.O. 99x generici elaborati
059000011211    3C                   if            V2CLNP =  V2CLNA
059100011210     C                             and V2CLNP >= £POX
059200011211     C                             and V2CATB =  *blanks
059300011211     C                   movel     *on           POX(V2CLNP)
059400011210    3C                   endif
059500980224      *
059600980305     C                   Z-ADD     NRR           REC
059700980224    2C                   END
059800980223     C*
059900980224     C     GIUCTR        TAG
060000980224     C   28              Z-ADD     NRR           REC
060100980224     C                   UPDATE    LV67S02
060200980224     C*
060300980224     C                   ADD       1             NRR
060400980224    1C  N28              ENDDO
060500011210      *
060600050421      * verifica esistenza baie per P.O. 999, 998, 997, 996, 995
060700011210     C  N28              eval      XX   = £POX
060800011210    1C  N28£POX          do        999
060900011210    2C                   if        POX(xx) = *off
061000011210    3C                   if        xx = 999
061100011210     C                   eval      wDes = 'TUTTE'
061200011210     C                   else
061300011210     C                   eval      wDes = TES(1000-XX)
061400011210    3C                   endif
061500011211     C                   clear                   wXXa
061600011210     C                   move      xx            wXXa
061700011210     C                   eval      VCCMSG = %trim(MSG(13)) +
061800011211     C                                      (' ')   + wXXa +
061900011211     C                                      (' - ') + %trim(wDes)
062000011210     C                   seton                                        4428
062100011211     C                   leave
062200011210    2C                   endif
062300011210     C                   add       1             XX
062400011210    1C                   enddo
062500980224      *
062600980224     C                   ENDSR
062700980224      *-------------------------------------------------------------------------
062800980224      * CONFERMO I DATI DEL SUBFILE
062900980224      *-------------------------------------------------------------------------
063000980224     C     CONSF1        BEGSR
063100020618      *
063200020618     C                   eval      $FLTR3xxx = *on
063300980224      *
063400980224      * In caso di manutenzione di un piano di smistamento già esistente
063500980224      * annullo i records esistenti sul file e scrivo quelli presenti
063600980224      * nel subfile
063700020618     C                   move      V1Cpog        wPOG
063800020618     C     K03TR350      SETLL     FLTR3500
063900020618     C     K03TR350      READE     FLTR3500                               33
064000980224     C     *IN33         DOWEQ     *OFF
064100020618     C                   DELETE    FLTR3500
064200020618     C     K03TR350      READE     FLTR3500                               33
064300980224     C                   ENDDO
064400980224      * scrittura nuovi records
064500980224     C                   Z-ADD     1             NRR
064600020618     C                   MOVE      V1CPOG        TR3POG
064700980224     C                   MOVE      V1CCTL        TR3CTL
064800980224     C                   MOVE      V1CNPL        TR3NPL
064900980224      *
065000980224    1C                   DO        *HIVAL
065100980224     C     NRR           CHAIN     LV67S02                            32
065200980224     C   32              LEAVE
065300980224      * se non e' annullato trasformo i campi numerici in alfanumerici
065400980224      * e scrivo il record in FLTR3
065500980227     C                   MOVE      V2SLNP        V2CLNP
065600980227     C                   MOVE      V2SLNA        V2CLNA
065700020618    2C     V2CATB        IFEQ      ' '
065800980224     C     V2CLNP        ANDNE     *ZEROS
065900980224      *
066000980224     C                   MOVE      V2CLNP        TR3LNP
066100980224     C                   MOVE      V2CLNA        TR3LNA
066200980224     C                   MOVE      V2CZON        TR3ZON
066300980224     C                   MOVE      '000'         TR3BAI
066400980224     C                   MOVE      V2CBAI        TR3BAI
066500980304     C                   MOVE      '1'           TR3PRB
066600980224      *
066700020618     C     K08TR350      CHAIN     FLTR3500
066800010320     C                   MOVEL     V2DDES        §tr3descri
066900010320     C                   MOVEL     V2splt        §tr3pallet
067000010320     C                   MOVEL     V2sopt        §tr3operat
067100010320     C                   MOVEL     V2spst        §tr3pista
067200010404     C                   eval      §tr3punto = '.'
067300010320     C                   MOVEL     dtr3des       tr3des
067400020618     C                   if        %found(FLTR351L)
067500020618     C                   UPDATE    FLTR3500
067600020618     C                   else
067700020618     C                   WRITE     FLTR3500
067800020618     C                   endif
067900020618   -2C                   ENDIF
068000980224      *
068100980303     C                   ADD       1             NRR
068200020618   -1C                   ENDDO
068300020618     C                   FEOD      FLTR351L
068400980303      *
068500980303      * VALORIZZO IL PROGRESSIVO DELLE BAIA DI USCITA IN BASE AL NUMERO DI VOLTE
068600980303      * CHE COMPARE LA STESSA LINEA PARTENZA / ARRIVO / ZONA
068700980304     C                   SETOFF                                           33
068800020618     C     K03TR350      SETLL     FLTR3500
068900020618    1C     *IN33         DOWEQ     *OFF
069000020618     C     K03TR350      READE     FLTR3500                               33
069100980224      *
069200020618     C     K06TR350      SETLL     FLTR3500
069300020618     C     K06TR350      READE     FLTR3500                               34
069400020618     C                   Z-ADD     1             WRKPRB
069500020618    2C     *IN34         DOWEQ     *OFF
069600020618     C     K06TR350      READE     FLTR3500                               34
069700020618     C   34K06TR350      SETGT     FLTR3500
069800980304      *
069900980304     C  N34              ADD       1             WRKPRB
070000980304     C  N34              MOVE      WRKPRB        TR3PRB
070100020618     C  N34              UPDATE    FLTR3500
070200980304      *
070300020618   -2C                   ENDDO
070400980304      *
070500020618   -1C                   ENDDO
070600980304      *
070700980224     C                   ENDSR
070800050421      **-------------------------------------------------------------**
070900050421      ** Inserimento automatico record in piano di smistamento       **
071000050421      **-------------------------------------------------------------**
071100050421     c     sr_InsAutom   BEGSR
071200050421      *
071300050421      * Inizializzazione videata
071400050421     c                   clear                   LV67W01
071500050421      *
071600050421      * Ciclo di gestione della videata
071700050421do  1c                   dow       NOT *inKF and NOT *inKL
071800050421     c                             or  *in90
071900050421      *
072000050421     c                   exfmt     LV67W01
072100050421     c                   clear                   W1Cmsg
072200050421     c                   setoff                                       28  90
072300050421     c                   setoff                                       484950
072400050421      * - F12=Ritorno
072500050421if  2c                   if        *inKL
072600050421     c                   leave
072700050421e   2c                   endif
072800050421      * - Controllo dati immessi
072900050421     c                   exsr      sr_CtrW01
073000050421      *
073100050421e   1c                   enddo
073200050421      *
073300050421      * F6=Conferma
073400050421if  1c                   if        *inKF and NOT *in90
073500050421     c                   exsr      sr_AddSflRec
073600050421e   1c                   endif
073700050421      *
073800050421     c                   ENDSR
073900050421      **-------------------------------------------------------------**
074000050421      ** Controllo dati inseriti in videata LV67W01                  **
074100050421      **-------------------------------------------------------------**
074200050421     c     sr_CtrW01     BEGSR
074300050421      *
074400050421      * P.O. Partenza
074500050421     c                   clear                   W1Dpop
074600050421      * - se *blanks lo considero *zeros (sarebbe numerico)
074700050421if  1c                   if        W1Cpop =  *blanks
074800050421     c                   eval      W1Cpop =  *zeros
074900050421e   1c                   endif
075000050421      * - ricerca
075100050421     c     '?'           scan      W1Cpop                                 30
075200050421if  1c                   if        *in30  =  *on
075300050421     c                   eval      W1Cpop =  *zeros
075400050421     c                   eval      Wcampo =  'W1CPOP    '
075500050421     c                   exsr      sr_Search
075600050421     c***                seton                                        48  90
075700050421e   1c                   endif
075800050421      * - controlli:
075900050421      * - - numericità
076000050421     c     DigitN        check     W1Cpop                                 30
076100050421if  1c                   if        *in30  =  *on
076200050421     c                   seton                                        482890
076300050421     c                   movel     Msg(3)        W1Cmsg
076400050421     c                   leavesr
076500050421e   1c                   endif
076600050421      * - - effettiva immissione
076700050421if  1c                   if        W1Cpop <= *zeros
076800050421     c                   seton                                        482890
076900050421     c                   movel     Msg(2)        W1Cmsg
077000050421     c                   leavesr
077100050421e   1c                   endif
077200050421      * - - validità
077300050421     c                   move      W1Cpop        Wpo
077400050421     c                   exsr      sr_CtrPO
077500050421     c                   movel     WpoD          W1Dpop
077600050421if  1c                   if        *in90  =  *on
077700050421     c                   seton                                        4828
077800050421     c                   movel     Msg(3)        W1Cmsg
077900050421     c                   leavesr
078000050421e   1c                   endif
078100050421      * - - non ammessi 995-998
078200050421if  1c                   if            Wpo >= £POX
078300050421     c                             and Wpo <  999
078400050421     c                   seton                                        482890
078500050421     c                   movel     Msg(16)       W1Cmsg
078600050421     c                   leavesr
078700050421e   1c                   endif
078800050421      *
078900050421      * P.O. Arrivo
079000050421     c                   clear                   W1Dpoa
079100050421      * - se *blanks lo considero *zeros (sarebbe numerico)
079200050421if  1c                   if        W1Cpoa =  *blanks
079300050421     c                   eval      W1Cpoa =  *zeros
079400050421e   1c                   endif
079500050421      * - ricerca
079600050421     c     '?'           scan      W1Cpoa                                 30
079700050421if  1c                   if        *in30  =  *on
079800050421     c                   eval      W1Cpoa =  *zeros
079900050421     c                   eval      Wcampo =  'W1CPOA    '
080000050421     c                   exsr      sr_Search
080100050421     c***                seton                                        49  90
080200050421e   1c                   endif
080300050421      * - controlli:
080400050421      * - - numericità
080500050421     c     DigitN        check     W1Cpoa                                 30
080600050421if  1c                   if        *in30  =  *on
080700050421     c                   seton                                        492890
080800050421     c                   movel     Msg(5)        W1Cmsg
080900050421     c                   leavesr
081000050421e   1c                   endif
081100050421      * - - effettiva immissione
081200050421if  1c                   if        W1Cpoa <= *zeros
081300050421     c                   seton                                        492890
081400050421     c                   movel     Msg(4)        W1Cmsg
081500050421     c                   leavesr
081600050421e   1c                   endif
081700050421      * - - validità
081800050421     c                   move      W1Cpoa        Wpo
081900050421     c                   exsr      sr_CtrPO
082000050421     c                   movel     WpoD          W1Dpoa
082100050421if  1c                   if        *in90  =  *on
082200050421     c                   seton                                        4928
082300050421     c                   movel     Msg(5)        W1Cmsg
082400050421     c                   leavesr
082500050421e   1c                   endif
082600050421      * - - non ammessi 995-998
082700050421if  1c                   if            Wpo >= £POX
082800050421     c                             and Wpo <  999
082900050421     c                   seton                                        492890
083000050421     c                   movel     Msg(16)       W1Cmsg
083100050421     c                   leavesr
083200050421e   1c                   endif
083300050421      *
083400050421      * Compatibilità P.O.
083500050421if  1c                   if            W1Cpop = '999'
083600050421     c                             and W1Cpoa = '999'
083700050421     c                   seton                                        482890
083800050421     c                   movel     Msg(17)       W1Cmsg
083900050421     c                   leavesr
084000050421e   1c                   endif
084100050421      *
084200050421      * Range zone
084300050427sel 1c                   select
084400050427w   1c                   when      W1Czoi > W1Czof
084500050421     c                   seton                                        502890
084600050421     c                   movel     Msg(18)       W1Cmsg
084700050421     c                   leavesr
084800050427w   1c                   when      W1Czoi = W1Czof
084900050427     c                   seton                                        502890
085000050427     c                   movel     Msg(19)       W1Cmsg
085100050427     c                   leavesr
085200050427e   1c                   endsl
085300050421      *
085400050421     c                   ENDSR
085500050421      **-------------------------------------------------------------**
085600050421      ** Aggiornamento SFL con nuovi record                          **
085700050421      **-------------------------------------------------------------**
085800050421     c     sr_AddSflRec  BEGSR
085900050421      *
086000050421      * Scorro il sfl per determinare la 1ª riga libera
086100050421     c                   eval      NRR = C_SflSiz
086200050421      *
086300050421do  1c                   dow       NRR > *zeros
086400050421      *
086500050421     c     NRR           chain     LV67S02                            32
086600050421      *
086700050421if  2c                   if        NOT *in32
086800050421      *
086900050421      * - Verifico se nella riga risultano inseriti dati
087000050421if  3c                   if            (V2Slnp <> *zeros
087100050421     c                             and  V2Slnp <> *blanks)
087200050421     c                             OR  (V2Slna <> *zeros
087300050421     c                             and  V2Slna <> *blanks)
087400050421     c                             OR   V2Czon <> *zeros
087500050421     c                             OR   V2Cbai <> *zeros
087600050421     c                   leave
087700050421e   3c                   endif
087800050421      *
087900050421e   2c                   endif
088000050421      *
088100050421     c                   sub       1             NRR
088200050421      *
088300050421e   1c                   enddo
088400050421      *
088500050421      * Aggiungo i records nel sfl (...finchè ce ne stanno)
088600050421     c                   eval      Wzona3  = W1Czoi
088700050421do  1c                   dow           NRR    >  *zeros
088800050421     c                             and NRR    <  C_SflSiz
088900050421     c                             and Wzona3 <= W1Czof
089000050421      *
089100050421     c                   eval      NRR = NRR + 1
089200050421     c     NRR           CHAIN     LV67S02
089300050421if  2c                   if        %found(FNLV67D)
089400050421     c                   clear                   LV67S02
089500050421     c                   eval      V2Slnp  = W1Cpop
089600050421     c                   movel     W1Dpop        V2Dlnp
089700050421     c                   eval      V2Slna  = W1Cpoa
089800050421     c                   movel     W1Dpoa        V2Dlna
089900050421     c                   eval      V2Czon  = Wzona3
090000050421     c                   add       1             Wzona3
090100050421     c                   eval      V2Cbai  = W1Cbai
090200050421     c                   UPDATE    LV67S02
090300050421e   2c                   endif
090400050421      *
090500050421e   1c                   enddo
090600050421      *
090700050421      * Segnalo?se?NON sono stati inseriti tutti i records nel sfl
090800050421      *  (1 rec. per zona)
090900050421if  1c                   if        Wzona3 <= W1Czof
091000050427     c                   movel     Msg(20)       VCCmsg
091100050422     c                   move      Wzona3        WzonaA
091200050422     c                   eval      VCCmsg = %trim(%replace(WzonaA:VCCmsg:
091300050422     c                                      %scan('&Z':VCCmsg)))
091400050422     c                   move      W1Czof        WzonaA
091500050422     c                   eval      VCCmsg = %trim(%replace(WzonaA:VCCmsg:
091600050422     c                                      %scan('&Z':VCCmsg)))
091700050421     c                   seton                                        412890
091800050421     c                   leavesr
091900050421e   1c                   endif
092000050421      *
092100050421     c                   ENDSR
092200050421      **-------------------------------------------------------------**
092300050421      ** Gestione ricerche                                           **
092400050421      **-------------------------------------------------------------**
092500050421     c     sr_Search     BEGSR
092600050421      *
092700050421sel 1c                   select
092800050421      *
092900050421      * P.O. (Partenza o Arrivo)
093000050421w   1c                   when          Wcampo =  'V2SLNP    '
093100050421     c                             or  Wcampo =  'V2SLNA    '
093200050421     c                             or  Wcampo =  'W1CPOP    '
093300050421     c                             or  Wcampo =  'W1CPOA    '
093400050421     c                   clear                   DsTNSD
093500050421     c                   call      'TNSD24R'
093600050421     c                   parm                    §cod
093700050421     c                   parm                    §tip
093800050421     c                   parm                    §des
093900050421     c                   parm                    §pra
094000050421sel 2c                   select
094100050421w   2c                   when      Wcampo = 'V2SLNP    '
094200050421     c                   movel     §cod          V2Slnp
094300050421     c                   movel     §des          V2Dlnp
094400050421w   2c                   when      Wcampo = 'V2SLNA    '
094500050421     c                   movel     §cod          V2Slna
094600050421     c                   movel     §des          V2Dlna
094700050421w   2c                   when      Wcampo = 'W1CPOP    '
094800050421     c                   movel     §cod          W1Cpop
094900050421     c                   movel     §des          W1Dpop
095000050421w   2c                   when      Wcampo = 'W1CPOA    '
095100050421     c                   movel     §cod          W1Cpoa
095200050421     c                   movel     §des          W1Dpoa
095300050421e   2c                   endsl
095400050421      *
095500050421e   1c                   endsl
095600050421      *
095700050421     c                   ENDSR
095800050421      **-------------------------------------------------------------**
095900050421      ** Controllo singolo P.O.                                      **
096000050421      **-------------------------------------------------------------**
096100050421     c     sr_CtrPO      BEGSR
096200050421      *
096300050421     c                   clear                   WpoD
096400050421      *
096500050421if  1c                   if        Wpo    <  £POX
096600050421      *
096700050421     c     Wpo           chain     AZORG
096800050421      *
096900050421if  2c                   if        NOT   %found(AZORG01L)
097000050421     c                             or    %found(AZORG01L)
097100050421     c                             and  ( ORGfva <> *blanks
097200050421     c                              or   (ORGfag <> 'A'
097300050421     c                              and   ORGfag <> 'F'))
097400050421     c                   eval      *in90  =  *on
097500050421x   2c                   else
097600050421if  3c                   if        ORGde5 <> *blanks
097700050421     c                   movel     ORGde5        WpoD
097800050421x   3c                   else
097900050421     c                   movel     ORGdes        WpoD
098000050421e   3c                   endif
098100050421e   2c                   endif
098200050421      *
098300050421x   1c                   else
098400050421      *
098500050421sel 2c                   select
098600050421w   2c                   when      Wpo = 999
098700050421     c                   movel(p)  'TUTTE'       WpoD
098800050421w   2c                   when      Wpo = 998
098900050421     c                   movel(p)  Tes(2)        WpoD
099000050421w   2c                   when      Wpo = 997
099100050421     c                   movel(p)  Tes(3)        WpoD
099200050421w   2c                   when      Wpo = 996
099300050421     c                   movel(p)  Tes(4)        WpoD
099400050421w   2c                   when      Wpo = 995
099500050421     c                   movel(p)  Tes(5)        WpoD
099600050421w   2c*** solo p.o.arr   when      Wpo = 990
099700050421     c***                movel(p)  Tes(6)        WpoD
099800050421e   2c                   endsl
099900050421      *
100000050421e   1c                   endif
100100050421      *
100200050421     c                   ENDSR
100300980224      *-------------------------------------------------------------------------
100400980224      * DUPLICAZIONE PIANO DI SMISTAMENTO IN UN ALTRO
100500980224      *-------------------------------------------------------------------------
100600980224     C     DUPTR         BEGSR
100700980224      *
100800020618     C                   MOVE      V1CPOG        V3CAOG
100900980224     C                   CLEAR                   V3CATL
101000980224     C                   CLEAR                   V3CAPL
101100020618     C                   MOVE      V1CPOG        V3CDOG
101200980224     C                   CLEAR                   V3CDTL
101300980224     C                   CLEAR                   V3CDPL
101400980224      *
101500980224    1C                   DO        *HIVAL
101600980224     C                   EXFMT     LV67D03
101700980224      *
101800980224      * pulizia campo messaggio e relativo indicatore  (*IN28)
101900980224     C                   CLEAR                   V3CMSG
102000980224     C                   SETOFF                                       28
102100980224      * azzero indicatori relativi ai messaggi di errore
102200980224     C                   SETOFF                                       4546
102300020618     C                   SETOFF                                       4849
102400980224      *
102500980224      ** Ritorno alla videata precedente F12
102600980224     C   KL              LEAVE
102700020618      *
102800020618      * F18=Cambio P.O. in gestione
102900020618     c   KS              eval      *in05 = *on
103000020618     c   KS              iter
103100020618      *
103200020618      * 05 on --> controllo P.O. da cui copiare
103300020618     c                   if        *in05
103400020618     c                   move      V3Cdog        WRKPOG
103500020618     c                   exsr      CTRFGS
103600020618     c   90              movel     *on           *in49
103700020618     c   90              movel     D50msg        V3Cmsg
103800020618     c   90              iter
103900020618     c                   eval      *in05 = *off
104000020618     c                   endif
104100980224      *
104200980224      ** Controllo piani di smistamento inseriti
104300980224      *  devono essere diversi da blanks
104400020618    2C     V3CAOG        IFEQ      *ZEROS
104500020618     C     V3CATL        OREQ      *BLANK
104600980224     C     V3CAPL        OREQ      *BLANK
104700980224     C                   MOVEL     MSG(10)       V3CMSG
104800980224     C                   SETON                                        4528
104900980224     C                   ITER
105000980224    2C                   ENDIF
105100980224      *
105200980224     C                   MOVEL     V3CAPL        WCTL
105300980224      * il primo carattere del piano di lavorazione deve essere uguale
105400980224      * al tipo lavorazione
105500980224    2C     WCTL          IFNE      V3CATL
105600980224     C                   MOVEL     MSG(1)        V3CMSG
105700980224     C                   SETON                                        4528
105800980224     C                   ITER
105900980224    2C                   ENDIF
106000980224      * Controllo se il piano di smistamento che deve essere creato esiste già
106100020618     C                   MOVE      V3CAOG        wPOG
106200980224     C                   MOVE      V3CATL        V1CCTL
106300980224     C                   MOVE      V3CAPL        V1CNPL
106400020618     C     K03TR350      CHAIN     FLTR3500                           33
106500980224    2C     *IN33         IFEQ      *OFF
106600980224     C                   SETON                                        4528
106700980224     C                   MOVEL     MSG(9)        V3CMSG
106800980224     C                   ITER
106900020618    2C                   ENDIF
107000980224      *  devono essere diversi da blanks
107100020618    2C     V3CDOG        IFEQ      *ZEROS
107200020618     C     V3CDTL        OREQ      *BLANK
107300980224     C     V3CDPL        OREQ      *BLANK
107400980224     C                   MOVEL     MSG(10)       V3CMSG
107500980224     C                   SETON                                        4628
107600980224     C                   ITER
107700980224    2C                   ENDIF
107800980224      *
107900980224     C                   MOVEL     V3CDPL        WCTL
108000980224      * il primo carattere del piano di lavorazione deve essere uguale
108100980224      * al tipo lavorazione
108200980224    2C     WCTL          IFNE      V3CDTL
108300980224     C                   MOVEL     MSG(1)        V3CMSG
108400980224     C                   SETON                                        4628
108500980224     C                   ITER
108600980224    2C                   ENDIF
108700980224      * Controllo se il piano di smistamento da duplicare non  esiste
108800020618     C                   MOVE      V3CDOG        wPOG
108900980224     C                   MOVE      V3CDTL        V1CCTL
109000980224     C                   MOVE      V3CDPL        V1CNPL
109100020618     C     K03TR350      SETLL     FLTR3500                               33
109200980224    2C     *IN33         IFEQ      *OFF
109300980224     C                   SETON                                        4628
109400980224     C                   MOVEL     MSG(11)       V3CMSG
109500980224     C                   ITER
109600020618    2C                   ENDIF
109700020618      ** Duplicazione Dati  F6
109800980224    2C     *INKF         IFEQ      *ON
109900980224    3C                   DO        *HIVAL
110000020618     C     K03TR350      READE     FLTR3500
110100020618    4C                   IF        not %eof(FLTR351L)
110200020618     C                   MOVE      V3CAOG        TR3POG
110300980224     C                   MOVE      V3CATL        TR3CTL
110400980224     C                   MOVE      V3CAPL        TR3NPL
110500020618     C                   WRITE     FLTR3500
110600020618     C                   else
110700020618     C                   leave
110800020618    4C                   ENDIF
110900020618    3C                   ENDDO
111000980224      * Ricarico il subfile con il nuovo piano di smistamento
111100020618     C                   MOVE      V3CAOG        V1CPOG
111200980224     C                   MOVE      V3CATL        V1CCTL
111300980224     C                   MOVE      V3CAPL        V1CNPL
111400980224     C                   EXSR      CARSF1
111500980224     C                   Z-ADD     1             REC
111600980224     C                   SETON                                        2021
111700980224     C                   WRITE     LV67Z02
111800980224      *
111900980224     C                   LEAVE
112000020618    2C                   ENDIF
112100980224      **
112200980224    1C                   ENDDO
112300980224      *
112400980224     C                   ENDSR
112500020724      *---------------------------------------------------------------
112600020724      * POSIZIONAMENTO CURSORE AL 1° RECORD DELLA PAGINA
112700020724      *---------------------------------------------------------------
112800020724     C     POSCUR        BEGSR
112900020724      *
113000020724     C                   IF        V2CPSZ > 0
113100020724     C                   Z-ADD     V2CPSZ        REC
113200020724     C                   ENDIF
113300020724      *
113400020724     C     REC           DIV       SFLPAG        PAGINE            4 0
113500020724     C                   MVR                     RESTO             3 0
113600020724     C     PAGINE        MULT      SFLPAG        REC
113700020724     C     RESTO         IFGT      0
113800020724     C                   ADD       1             REC
113900020724     C                   ELSE
114000020724     C                   SUB       SFLPAG        REC
114100020724     C                   ADD       1             REC
114200020724     C                   ENDIF
114300020724      *
114400020724     C                   ENDSR
114500980224** TESTATA PROGRAMMA
114600980224MANUTENZIONE  PIANI DI SMISTAMENTO
114700980227NON LETTI
114800000221NON QUADRATI
114900000719NON STAMPATI
115000011210INVERSIONI
115100040924FORZ.INSTRAD.
115200980223** MESSAGGI ERRORI
115300980224Primo carattere del piano lavorazione deve essere uguale al tipo lavorazione   1
115400980224Inserire la linea di partenza                                                  2
115500980224Linea di partenza inesistente                                                  3
115600980224Inserire la linea di arrivo                                                    4
115700980224Linea di arrivo inesistente                                                    5
115800980224Zona errata                                                                    6
115900980224Inserire la baia                                                               7
116000980224Baia errata                                                                    8
116100980224Piano di smistamento già esistente impossibile crearlo                         9
116200980224Inserimento obbligatorio dei parametri di duplicazione                        10
116300980224Piano di smistamento non esiste impossibile duplicarlo                        11
116400011210Inserire il tipo lavorazione                                                  12
116500011210Manca la baia                                                                 13
116600020724Solo ammessi solo valori numerici                                             14
116700050420Inserito carattere non valido: ' (apice)                                      15
116800050421Per questa funzione NON sono ammessi P.O. dal 995 al 998 (riservati)          16
116900050421Impossibile immettere entrambi i P.O. (di partenza e di arrivo) "999"         17
117000050421Zona iniziale > Zona finale                                                   18
117100050427Immettere più di una zona                                                     19
117200050427Spazio insufficiente per i record "automatici" dalla zona &Z alla zona &Z     20
117300080526Utente NON abilitato. Premere Invio per uscire dal programma.                 21
