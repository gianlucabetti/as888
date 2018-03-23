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
006400070109     d C_SflSiz        c                   const(1500)
006500020724     D* caratteri numerici consentiti
006600020724     D DigitN          C                   CONST('1234567890')
006700010320
006800020618      *****************************************************************
006900020618      * RIEPILOGO INDICATORI
007000020618      *****************************************************************
007100020618      * 05    - SPROTEGGO P.O. IN GESTIONE
007200020618      * 06    - NON ABILITO TASTO FUNZIONALE F18=Cambio P.O.
007300020618      * 20    - SFLDSP
007400020618      * 21    - SFLDSPCTL
007500020618      * 24    - SFLINZ
007600020618      * 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
007700020618      * 31/33 - DI COMODO
007800020618      * 40/48 - ERRORI
007900020618      *****************************************************************
008000050426      *
008100020618      *---------------------------------------------------------------*
008200050426      *
008300020618      * DEFINIZIONE CHIAVI
008400020618      *
008500020618     C     K03TR350      KLIST
008600020618     C                   KFLD                    wPOG
008700020618     C                   KFLD                    V1CCTL
008800020618     C                   KFLD                    V1CNPL
008900020618      *
009000020618     C     K08TR350      KLIST
009100020618     C                   KFLD                    TR3POG
009200020618     C                   KFLD                    TR3CTL
009300020618     C                   KFLD                    TR3NPL
009400020618     C                   KFLD                    TR3LNP
009500020618     C                   KFLD                    TR3LNA
009600020618     C                   KFLD                    TR3ZON
009700020618     C                   KFLD                    TR3PRB
009800020618     C                   KFLD                    TR3BAI
009900020618      *
010000020618     C     K06TR350      KLIST
010100020618     C                   KFLD                    TR3POG
010200020618     C                   KFLD                    TR3CTL
010300020618     C                   KFLD                    TR3NPL
010400020618     C                   KFLD                    TR3LNP
010500020618     C                   KFLD                    TR3LNA
010600020618     C                   KFLD                    TR3ZON
010700050426      *
010800020618      *---------------------------------------------------------------*
010900050426      *
011000980223     C     *ENTRY        PLIST
011100980223     C                   PARM                    KPJBA
011200050426      *
011300050421      * Reperimento dati aziendali
011400050421     c     *dtaara       define    §azute        AZUTEds
011500050421     c     *dtaara       define    §datiute      dDATIUTE
011600050421     c                   in(E)     *dtaara
011700050421     c                   if        %ERROR or RSUT = *blanks
011800050421     c                   clear                   Tibs34Ds
011900050421     c                   call      'TIBS34R'
012000050421     c                   parm                    Tibs34Ds
012100050421     c                   in        *dtaara
012200050421     c                   endif
012300050421     c                   movel     DUTdpo        VTCrsu
012400080526     c                   movel     UTEfaf        dUte01
012500050426      *
012600980223      * TESTATA PGM
012700980224     C                   MOVEL     TES(1)        VTCTES
012800020618      *
012900020618      * Imposto il "P.O. IN GESTIONE"
013000050421     c                   move      dutPOU        V1Cpog
013100050421if  1c                   IF        dutLPO = '2' or dutLPO = *blanks
013200020618     c                   eval      *in06  = *on
013300020618e   1c                   ENDIF
013400020618     c                   move      V1Cpog        WRKPOG
013500020618     c                   exsr      CTRFGS
013600020618     c                   movel     ORGdes        V1Dpog
013700020618     c                   SETOFF                                       9028
013800080526
013900080526      * Se utente NON abilitato: visualizza messaggio per uscire
014000080526      *   subito dal programma
014100080526     c                   if        §UTEmps <> 'S'
014200080526     c                   eval      *in28   =  *on
014300080526     c                   eval      V1CMsg  =  Msg(21)
014400080526     c                   endif
014500050426      *
014600020618      *---------------------------------------------------------------*
014700050426      *
014800980223     C     INIZIO        TAG
014900980223     C                   CLEAR                   V1CCTL
015000980223     C                   CLEAR                   V1CNPL
015100980223      *
015200980223     C     FOR01         TAG
015300980223     C                   WRITE     LV67T01
015400980223     C                   EXFMT     LV67D01
015500980223      *
015600980223      * pulizia campo messaggio e relativo indicatore  (*IN28)
015700980224     C                   CLEAR                   V1CMSG
015800980223      * azzero indicatori relativi ai messaggi di errore
015900050426     C                   SETOFF                                       28  90
016000050426     C                   SETOFF                                       4048
016100080526      *
016200080526      * Utente non abilitato
016300080624     c     §UTEmps       cabne     'S'           Fine
016400980223      *
016500050426      * F3=Fine lavoro
016600980223     C   KC              GOTO      FINE
016700020618      *
016800020618      * F18=Cambio P.O. in gestione
016900020618     c   KS              eval      *in05 = *on
017000020618     c   KS              goto      FOR01
017100020618      *
017200020618      * 05 on --> controllo P.O. in gestione
017300020618     c                   if        *in05
017400020618     C                   clear                   V1Dpog
017500020618     c                   move      V1Cpog        WRKPOG
017600020618     c                   exsr      CTRFGS
017700020618     c   90              movel     D50MSG        V1CMSG
017800020618     c   90              goto      FOR01
017900020618     c                   movel     ORGdes        V1Dpog
018000020618     c                   eval      *in05 = *off
018100020618     c                   endif
018200980223      *
018300050426      * Controllo dati del formato D01
018400050426     c                   exsr      CtrD01
018500050426     c   28              goto      FOR01
018600050426      *
018700020618      *---------------------------------------------------------------*
018800050426      *
018900980223      * GESTIONE VIDEATA MANUTENZIONE
019000980223     C     FOR02         TAG
019100980223     C                   EXSR      CARSF1
019200980223     C                   EXSR      GESSF1
019300980224      *
019400980224     C                   GOTO      INIZIO
019500050426      *
019600020618      *---------------------------------------------------------------*
019700050426      *
019800980224     C     FINE          TAG
019900020618     C                   if        $FLTR3xxx = *on
020000020618     C                   clear                   KPJBU
020100020618     C                   call      'FNLV67R2'
020200020618     C                   parm                    KPJBA
020300020618     C                   endif
020400980224     C                   SETON                                        LR
020500020618      **********************************************************************
020600020618      * CONTROLLO IL P.O. IN IN GESTIONE
020700020618      **********************************************************************
020800020618     C     CTRFGS        BEGSR
020900020618      *
021000020618     C                   setoff                                       9028
021100020618     C                   clear                   FNLV50DS
021200020618     C                   movel     KNMUS         D50PRU
021300020618     C                   move      WRKPOG        D50FGS
021400020618     C                   call      'FNLV50R'
021500020618     C                   parm                    FNLV50DS
021600020618      *
021700020618if  1C                   IF        D50ERR <> *blanks
021800020618      *
021900020618     C                   seton                                        9028
022000020618      *
022100020618x   1C                   else
022200020618      *
022300020618      * decodifica
022400020618     C     WRKPOG        chain     AZORG
022500020618if  2C                   IF        not %found(AZORG01L)
022600020618     C                   clear                   orgDES
022700020618e   2C                   ENDIF
022800020618      *
022900020618e   1C                   ENDIF
023000020618      *
023100020618     C                   ENDSR
023200050426
023300050426      *---------------------------------------------------------------*
023400050426      * CTRD01 * CONTROLLO DATI IN FORMATO D01                        *
023500050426      *---------------------------------------------------------------*
023600050426     c     CtrD01        BEGSR
023700050426      *
023800050426      * Tipo lavorazione
023900050426      * - effettiva immissione
024000050426     c                   if           V1Cctl = *blanks
024100050426     c                             or V1Cctl = *zeros
024200050426     c                   movel     MSG(12)       V1Cmsg
024300050426     c                   seton                                        48  28
024400050426     c                   goto      EndCtrD01
024500050426     c                   endif
024600050426      * - controllo effettiva numericità del campo
024700050426     c     DigitN        check     V1Cctl                                 28
024800050426     c                   if        *in28 = *on
024900050426     c                   seton                                        48
025000050426     c                   movel     MSG(14)       V1Cmsg
025100050426     c                   goto      EndCtrD01
025200050426     c                   endif
025300050426      *
025400050426      * Piano lavorazione
025500050426      * - il primo carattere del piano di lavorazione deve essere uguale
025600050426      *   al tipo lavorazione
025700050426     c                   movel     V1Cnpl        wCTL
025800050426     c                   if        wCTL <> V1Cctl
025900050426     c                   movel     MSG(1)        V1Cmsg
026000050426     c                   seton                                        40  28
026100050426     c                   goto      EndCtrD01
026200050426     c                   endif
026300050426      * - controllo effettiva numericità del campo
026400050426     c     DigitN        check     V1Cnpl                                 28
026500050426     c                   if        *in28 = *on
026600050426     c                   seton                                        40
026700050426     c                   movel     MSG(14)       V1Cmsg
026800050426     c                   goto      EndCtrD01
026900050426     c                   endif
027000050426      *
027100050426     c     EndCtrD01     ENDSR
027200980223      *-------------------------------------------------------------------------
027300980223      * CARICAMENTO SUBFILE
027400980223      *-------------------------------------------------------------------------
027500980223     C     CARSF1        BEGSR
027600980223      * Pulizia ed inizializzazione subfile
027700980224     C                   SETOFF                                       2021
027800980223     C                   SETON                                        24
027900980223     C                   WRITE     LV67C02
028000980223     C                   SETOFF                                       24
028100020618     C                   Z-ADD     *ZEROS        NRR
028200020724     C                   clear                   V2CPSZ
028300980223      * Caricamento subfile
028400020618     C                   move      V1Cpog        wPOG
028500020618     C     K03TR350      SETLL     FLTR3500
028600020618     C     K03TR350      READE     FLTR3500                               31
028700980223      * se non esiste alcun record scrivo nel subfile i record di mancata
028800000221      * lettura e zona non prevista e altri 2
028900980223    1C     *IN31         IFEQ      *ON
029000980223     C                   ADD       1             NRR
029100980224     C     NRR           CHAIN     LV67S02                            32
029200980223    2C  N32              DO
029300980227     C                   MOVE      '999'         V2SLNP
029400980227     C                   MOVE      '999'         V2SLNA
029500980223     C                   Z-ADD     99            V2CZON
029600980223     C                   Z-ADD     *ZEROS        V2CBAI
029700980223     C                   MOVE      *BLANKS       V2DLNP
029800980227     C                   MOVEL     'TUTTE'       V2DLNP
029900980223     C                   MOVE      *BLANKS       V2DLNA
030000980227     C                   MOVEL     'TUTTE'       V2DLNA
030100980223     C                   MOVE      *BLANKS       V2CATB
030200980224     C                   UPDATE    LV67S02
030300980223    2C                   END
030400980223     C*
030500980223     C                   ADD       1             NRR
030600980224     C     NRR           CHAIN     LV67S02                            32
030700980223    2C  N32              DO
030800980227     C                   MOVE      '998'         V2SLNP
030900980227     C                   MOVE      '998'         V2SLNA
031000980223     C                   Z-ADD     99            V2CZON
031100980223     C                   Z-ADD     *ZEROS        V2CBAI
031200980223     C                   MOVE      *BLANKS       V2DLNP
031300980227     C                   MOVEL     TES(2)        V2DLNP
031400980223     C                   MOVE      *BLANKS       V2DLNA
031500980227     C                   MOVEL     TES(2)        V2DLNA
031600980223     C                   MOVE      *BLANKS       V2CATB
031700980224     C                   UPDATE    LV67S02
031800000221    2C                   END
031900000221     C*
032000000221     C                   ADD       1             NRR
032100000221     C     NRR           CHAIN     LV67S02                            32
032200000221    2C  N32              DO
032300000221     C                   MOVE      '997'         V2SLNP
032400000221     C                   MOVE      '997'         V2SLNA
032500000221     C                   Z-ADD     99            V2CZON
032600000221     C                   Z-ADD     *ZEROS        V2CBAI
032700000221     C                   MOVE      *BLANKS       V2DLNP
032800000221     C                   MOVEL     TES(3)        V2DLNP
032900000221     C                   MOVE      *BLANKS       V2DLNA
033000000221     C                   MOVEL     TES(3)        V2DLNA
033100000221     C                   MOVE      *BLANKS       V2CATB
033200000221     C                   UPDATE    LV67S02
033300000221    2C                   END
033400000221     C                   ADD       1             NRR
033500000221     C     NRR           CHAIN     LV67S02                            32
033600000221    2C  N32              DO
033700000221     C                   MOVE      '996'         V2SLNP
033800000221     C                   MOVE      '996'         V2SLNA
033900000221     C                   Z-ADD     99            V2CZON
034000000221     C                   Z-ADD     *ZEROS        V2CBAI
034100000221     C                   MOVE      *BLANKS       V2DLNP
034200000221     C                   MOVEL     TES(4)        V2DLNP
034300000221     C                   MOVE      *BLANKS       V2DLNA
034400000221     C                   MOVEL     TES(4)        V2DLNA
034500000221     C                   MOVE      *BLANKS       V2CATB
034600000221     C                   UPDATE    LV67S02
034700000221    2C                   END
034800011210     C                   ADD       1             NRR
034900011210     C     NRR           CHAIN     LV67S02                            32
035000011210    2C  N32              DO
035100011210     C                   MOVE      '995'         V2SLNP
035200011210     C                   MOVE      '995'         V2SLNA
035300011210     C                   Z-ADD     99            V2CZON
035400011210     C                   Z-ADD     *ZEROS        V2CBAI
035500011210     C                   MOVE      *BLANKS       V2DLNP
035600011210     C                   MOVEL     TES(5)        V2DLNP
035700011210     C                   MOVE      *BLANKS       V2DLNA
035800011210     C                   MOVEL     TES(5)        V2DLNA
035900011210     C                   MOVE      *BLANKS       V2CATB
036000011210     C                   UPDATE    LV67S02
036100011210    2C                   END
036200980223      *
036300980223   X1C                   ELSE
036400980223      **** Se il tipo di lavorazione ed il piano esistono carico sul subfile
036500980223      **** in manutenzione
036600980223    2C     *IN31         DOWEQ     *OFF
036700980224     C                   ADD       1             NRR
036800980224     C     NRR           CHAIN     LV67S02                            32
036900980227     C                   MOVE      TR3LNP        V2SLNP
037000980227     C                   MOVE      TR3LNP        V2CLNP
037100980227     C                   MOVE      TR3LNA        V2SLNA
037200980227     C                   MOVE      TR3LNA        V2CLNA
037300980223     C                   MOVE      TR3ZON        V2CZON
037400980223     C                   MOVE      TR3BAI        V2CBAI
037500010320     C                   MOVEl     TR3DES        dtr3des
037600010320     C                   eval      v2spst = §tr3pista
037700010320     c                   eval      v2sopt = §tr3operat
037800010320     c                   eval      v2splt = §tr3pallet
037900010320     c                   movel     §tr3descri    v2ddes
038000980223      **** Cerco decodifica linea di partenza e linea di arrivo
038100050421     c                   move      V2Clnp        Wpo
038200050421     c                   exsr      sr_CtrPO
038300050421     c                   movel     WpoD          V2Dlnp
038400050421     c                   move      V2Clna        Wpo
038500050421     c                   exsr      sr_CtrPO
038600050421     c                   movel     WpoD          V2Dlna
038700040924     C     V2CLNA        IFEQ      990
038800040924     C                   MOVEL     *BLANKS       V2DLNA
038900040924     C                   MOVEL     TES(6)        V2DLNA
039000040924     C                   END
039100980223     C                   MOVE      *BLANKS       V2CATB
039200980223      * scrivo il subfile
039300980224     C   32              WRITE     LV67S02
039400980224     C  N32              UPDATE    LV67S02
039500980223      *
039600020618     C     K03TR350      READE     FLTR3500                               31
039700980223    2C                   ENDDO
039800050426    1C                   ENDIF
039900980223     C                   ENDSR
040000980223      *-------------------------------------------------------------------------
040100980223      * GESTIONE SUBFILE
040200980223      *-------------------------------------------------------------------------
040300980223     C     GESSF1        BEGSR
040400980223      *
040500980311     C                   WRITE     LV67Z02
040600980223     C                   Z-ADD     1             REC
040700980223     C                   SETON                                        2021
040800980223      *
040900980224    1C                   DO        *HIVAL
041000020724     C                   EXSR      POSCUR
041100980224     C                   EXFMT     LV67C02
041200980224      * pulizia campo messaggio e relativo indicatore  (*IN28)
041300980223     C                   CLEAR                   VCCMSG
041400050421     C                   SETOFF                                       28  90
041500980224      * azzero indicatori relativi ai messaggi di errore
041600980224     C                   SETOFF                                       414243
041700050420     C                   SETOFF                                       4447
041800050421      * F12=Ritorno
041900050421if  2c                   if        *inKL = *on
042000050421     c                   leave
042100050421e   2c                   endif
042200050421      * gestione videata di inserim. dati    F8
042300050421if  2c                   if        *inKH = *on
042400050421     c                   exsr      sr_InsAutom
042500050421     c                   iter
042600050421e   2c                   endif
042700980224      * gestione videata della duplicazione  F13
042800980224    2C     *INKM         IFEQ      *ON
042900980224     C                   EXSR      DUPTR
043000980311     C                   WRITE     LV67Z02
043100980224     C                   ITER
043200980224    2C                   ENDIF
043300980224      * richiamo programma di stampa         F10
043400980224    2C     *INKJ         IFEQ      *ON
043500020618     C                   MOVE      V1CPOG        PA1POG
043600980305     C                   MOVE      V1CCTL        PA1CTL
043700980305     C                   MOVE      V1CNPL        PA1DPL
043800980305     C                   MOVE      V1CNPL        PA1APL
043900980305     C                   MOVEL     PARAM1        KPJBU
044000980305     C                   MOVEL     'LV69'        KCOAZ
044100980305     C                   CALL      'BCH10'
044200980305     C                   PARM                    KPJBA
044300980305      *
044400980305     C                   ITER
044500980224    2C                   ENDIF
044600980223      * controllo dati inseriti a video      Enter
044700980224     C                   EXSR      CTRSF1
044800980224      * errori
044900980224     C   28              ITER
045000980224      * gestione conferma dati               F06
045100980224    2C     *INKF         IFEQ      *ON
045200980224     C                   EXSR      CONSF1
045300980224     C                   LEAVE
045400980224    2C                   ENDIF
045500980224      *
045600980224    1C                   ENDDO
045700980224      *
045800980224     C                   ENDSR
045900980224      *-------------------------------------------------------------------------
046000980224      * CONTROLLO DATI SUBFILE
046100980224      *-------------------------------------------------------------------------
046200980224     C     CTRSF1        BEGSR
046300980223     C                   Z-ADD     1             NRR
046400011210     C                   RESET                   POX
046500980223      *
046600980224    1C                   DO        *HIVAL
046700980224      * Spengo gli indicatori di errore
046800050421     C                   SETOFF                                       28  90
046900050421     C                   SETOFF                                       414243
047000050421     C                   SETOFF                                       444750
047100980224      *
047200980224     C     NRR           CHAIN     LV67S02                            32
047300980224     C   32              LEAVE
047400020618      *
047500020618     C                   if        V2Catb = 'A'
047600020618     C                   clear                   V2Dlnp
047700020618     C                   clear                   V2Dlna
047800020618     C                   goto      GIUCTR
047900020618     C                   endif
048000980224      * controllo se inserito almeno un campo della riga e se valido
048100980227     C     V2SLNP        IFNE      *ZEROS
048200980227     C     V2SLNP        ANDNE     *BLANKS
048300980227     C                   SETON                                        50
048400980227     C                   END
048500980227     C  N50V2SLNA        IFNE      *ZEROS
048600980227     C     V2SLNA        ANDNE     *BLANKS
048700980227     C                   SETON                                        50
048800980227     C                   END
048900980224     C  N50V2CZON        COMP      *ZEROS                             5050
049000980224     C  N50V2CBAI        COMP      *ZEROS                             5050
049100020724     C                   clear                   V2Dlnp
049200020724     C                   clear                   V2Dlna
049300980224      * almeno un campo e' stato inserito
049400980224    2C     *IN50         IFEQ      *ON
049500980224      * Controllo linea di partenza
049600980227    3C     V2SLNP        IFNE      *ZEROS
049700980227     C     V2SLNP        ANDNE     *BLANKS
049800980227     C*
049900980227     C     '?'           SCAN      V2SLNP                                 30
050000980227     C*
050100980227     C     *IN30         IFEQ      *ON
050200050421     c                   eval      Wcampo =  'V2SLNP    '
050300050421     c                   exsr      sr_Search
050400050421     C                   SETON                                        41
050500050421     C                   GOTO      GIUCTR
050600050421     C                   ENDIF
050700980224    3C                   END
050800050421      *
050900050421     C                   MOVE      V2SLNP        V2CLNP            3 0
051000050421     c                   move      V2Slnp        Wpo
051100050421     c                   exsr      sr_CtrPO
051200050421     c                   movel     WpoD          V2Dlnp
051300050421      * *in90 => P.O. partenza inesistente
051400050421if  3c                   if        *in90
051500050422     c                   eval      V2Cpsz =  NRR
051600050421     c                   movel     Msg(3)        VCCmsg
051700050421     c                   seton                                        4128
051800050421     c                   clear                   V2Dlnp
051900050421     c                   goto      GIUCTR
052000050421e   3c                   endif
052100980224     C*
052200980227      * Controllo linea di arrivo
052300980227    3C     V2SLNA        IFNE      *ZEROS
052400980227     C     V2SLNA        ANDNE     *BLANKS
052500980227     C*
052600980227     C     '?'           SCAN      V2SLNA                                 30
052700980227     C*
052800980227     C     *IN30         IFEQ      *ON
052900050421     c                   eval      Wcampo =  'V2SLNA    '
053000050421     c                   exsr      sr_Search
053100980227     C                   SETON                                        42
053200980227     C                   GOTO      GIUCTR
053300980227     C                   ENDIF
053400980227    3C                   END
053500050421      *
053600050421     C                   MOVE      V2SLNA        V2CLNA            3 0
053700050421     c                   move      V2Slna        Wpo
053800050421     c                   exsr      sr_CtrPO
053900050421     c                   movel     WpoD          V2Dlna
054000040924     C     V2CLNA        IFEQ      990
054100040924     C                   MOVEL     *BLANKS       V2DLNA
054200040924     C                   MOVEL     TES(6)        V2DLNA
054300050421     c                   else
054400050421      * *in90 => P.O. arrivo inesistente
054500050421if  4c                   if        *in90
054600050422     c                   eval      V2Cpsz =  NRR
054700050421     c                   movel     Msg(5)        VCCmsg
054800050421     c                   seton                                        4228
054900050421     c                   clear                   V2Dlna
055000050421     c                   goto      GIUCTR
055100050421e   4c                   endif
055200040924     C                   END
055300980227     C*
055400980224      * Controllo linea di arrivo
055500980224      * Controllo Zona ..... deve essere <= 99
055600980224    3C     V2CZON        IFGT      99
055700050422     c                   eval      V2Cpsz =  NRR
055800980224     C                   MOVEL     MSG(6)        VCCMSG
055900980224     C                   SETON                                        4328
056000980224     C                   GOTO      GIUCTR
056100980224    3C                   END
056200990129      * Controllo Baia ..... deve essere > 0 <= 45
056300010613      * dal 13/06/01 il controllo va spostato a 60 (Bocchi)
056400980224    3C     V2CBAI        IFEQ      *ZEROS
056500050422     c                   eval      V2Cpsz =  NRR
056600980224     C                   MOVEL     MSG(7)        VCCMSG
056700980224     C                   SETON                                        4428
056800980224     C                   GOTO      GIUCTR
056900980224    3C                   END
057000980224      *
057100010613    3C     V2CBAI        IFGT      60
057200050422     c                   eval      V2Cpsz =  NRR
057300980224     C                   MOVEL     MSG(8)        VCCMSG
057400980224     C                   SETON                                        4428
057500980224     C                   GOTO      GIUCTR
057600980224    3C                   END
057700050420      * controllo che nella descrizione non sia stato immesso il carattere apice
057800050420      * questo carattere non va bene x il pc
057900050420     c                   eval      wpos = %scan(wapice:v2ddes)
058000050420     c                   if        wpos > 0
058100050422     c                   eval      V2Cpsz =  NRR
058200050420     c                   eval      vccmsg = msg(15)
058300050420     c                   eval      *in28 = *on
058400050420     c                   eval      *in47 = *on
058500050420     c                   goto      giuctr
058600050420     c                   endif
058700011210      *
058800011210      * memorizzo i P.O. 99x generici elaborati
058900011211    3C                   if            V2CLNP =  V2CLNA
059000011210     C                             and V2CLNP >= £POX
059100011211     C                             and V2CATB =  *blanks
059200011211     C                   movel     *on           POX(V2CLNP)
059300011210    3C                   endif
059400980224      *
059500980305     C                   Z-ADD     NRR           REC
059600980224    2C                   END
059700980223     C*
059800980224     C     GIUCTR        TAG
059900980224     C   28              Z-ADD     NRR           REC
060000980224     C                   UPDATE    LV67S02
060100980224     C*
060200980224     C                   ADD       1             NRR
060300980224    1C  N28              ENDDO
060400011210      *
060500050421      * verifica esistenza baie per P.O. 999, 998, 997, 996, 995
060600011210     C  N28              eval      XX   = £POX
060700011210    1C  N28£POX          do        999
060800011210    2C                   if        POX(xx) = *off
060900011210    3C                   if        xx = 999
061000011210     C                   eval      wDes = 'TUTTE'
061100011210     C                   else
061200011210     C                   eval      wDes = TES(1000-XX)
061300011210    3C                   endif
061400011211     C                   clear                   wXXa
061500011210     C                   move      xx            wXXa
061600011210     C                   eval      VCCMSG = %trim(MSG(13)) +
061700011211     C                                      (' ')   + wXXa +
061800011211     C                                      (' - ') + %trim(wDes)
061900011210     C                   seton                                        4428
062000011211     C                   leave
062100011210    2C                   endif
062200011210     C                   add       1             XX
062300011210    1C                   enddo
062400980224      *
062500980224     C                   ENDSR
062600980224      *-------------------------------------------------------------------------
062700980224      * CONFERMO I DATI DEL SUBFILE
062800980224      *-------------------------------------------------------------------------
062900980224     C     CONSF1        BEGSR
063000020618      *
063100020618     C                   eval      $FLTR3xxx = *on
063200980224      *
063300980224      * In caso di manutenzione di un piano di smistamento già esistente
063400980224      * annullo i records esistenti sul file e scrivo quelli presenti
063500980224      * nel subfile
063600020618     C                   move      V1Cpog        wPOG
063700020618     C     K03TR350      SETLL     FLTR3500
063800020618     C     K03TR350      READE     FLTR3500                               33
063900980224     C     *IN33         DOWEQ     *OFF
064000020618     C                   DELETE    FLTR3500
064100020618     C     K03TR350      READE     FLTR3500                               33
064200980224     C                   ENDDO
064300980224      * scrittura nuovi records
064400980224     C                   Z-ADD     1             NRR
064500020618     C                   MOVE      V1CPOG        TR3POG
064600980224     C                   MOVE      V1CCTL        TR3CTL
064700980224     C                   MOVE      V1CNPL        TR3NPL
064800980224      *
064900980224    1C                   DO        *HIVAL
065000980224     C     NRR           CHAIN     LV67S02                            32
065100980224     C   32              LEAVE
065200980224      * se non e' annullato trasformo i campi numerici in alfanumerici
065300980224      * e scrivo il record in FLTR3
065400980227     C                   MOVE      V2SLNP        V2CLNP
065500980227     C                   MOVE      V2SLNA        V2CLNA
065600020618    2C     V2CATB        IFEQ      ' '
065700980224     C     V2CLNP        ANDNE     *ZEROS
065800980224      *
065900980224     C                   MOVE      V2CLNP        TR3LNP
066000980224     C                   MOVE      V2CLNA        TR3LNA
066100980224     C                   MOVE      V2CZON        TR3ZON
066200980224     C                   MOVE      '000'         TR3BAI
066300980224     C                   MOVE      V2CBAI        TR3BAI
066400980304     C                   MOVE      '1'           TR3PRB
066500980224      *
066600020618     C     K08TR350      CHAIN     FLTR3500
066700010320     C                   MOVEL     V2DDES        §tr3descri
066800010320     C                   MOVEL     V2splt        §tr3pallet
066900010320     C                   MOVEL     V2sopt        §tr3operat
067000010320     C                   MOVEL     V2spst        §tr3pista
067100010404     C                   eval      §tr3punto = '.'
067200010320     C                   MOVEL     dtr3des       tr3des
067300020618     C                   if        %found(FLTR351L)
067400020618     C                   UPDATE    FLTR3500
067500020618     C                   else
067600020618     C                   WRITE     FLTR3500
067700020618     C                   endif
067800020618   -2C                   ENDIF
067900980224      *
068000980303     C                   ADD       1             NRR
068100020618   -1C                   ENDDO
068200020618     C                   FEOD      FLTR351L
068300980303      *
068400980303      * VALORIZZO IL PROGRESSIVO DELLE BAIA DI USCITA IN BASE AL NUMERO DI VOLTE
068500980303      * CHE COMPARE LA STESSA LINEA PARTENZA / ARRIVO / ZONA
068600980304     C                   SETOFF                                           33
068700020618     C     K03TR350      SETLL     FLTR3500
068800020618    1C     *IN33         DOWEQ     *OFF
068900020618     C     K03TR350      READE     FLTR3500                               33
069000980224      *
069100020618     C     K06TR350      SETLL     FLTR3500
069200020618     C     K06TR350      READE     FLTR3500                               34
069300020618     C                   Z-ADD     1             WRKPRB
069400020618    2C     *IN34         DOWEQ     *OFF
069500020618     C     K06TR350      READE     FLTR3500                               34
069600020618     C   34K06TR350      SETGT     FLTR3500
069700980304      *
069800980304     C  N34              ADD       1             WRKPRB
069900980304     C  N34              MOVE      WRKPRB        TR3PRB
070000020618     C  N34              UPDATE    FLTR3500
070100980304      *
070200020618   -2C                   ENDDO
070300980304      *
070400020618   -1C                   ENDDO
070500980304      *
070600980224     C                   ENDSR
070700050421      **-------------------------------------------------------------**
070800050421      ** Inserimento automatico record in piano di smistamento       **
070900050421      **-------------------------------------------------------------**
071000050421     c     sr_InsAutom   BEGSR
071100050421      *
071200050421      * Inizializzazione videata
071300050421     c                   clear                   LV67W01
071400050421      *
071500050421      * Ciclo di gestione della videata
071600050421do  1c                   dow       NOT *inKF and NOT *inKL
071700050421     c                             or  *in90
071800050421      *
071900050421     c                   exfmt     LV67W01
072000050421     c                   clear                   W1Cmsg
072100050421     c                   setoff                                       28  90
072200050421     c                   setoff                                       484950
072300050421      * - F12=Ritorno
072400050421if  2c                   if        *inKL
072500050421     c                   leave
072600050421e   2c                   endif
072700050421      * - Controllo dati immessi
072800050421     c                   exsr      sr_CtrW01
072900050421      *
073000050421e   1c                   enddo
073100050421      *
073200050421      * F6=Conferma
073300050421if  1c                   if        *inKF and NOT *in90
073400050421     c                   exsr      sr_AddSflRec
073500050421e   1c                   endif
073600050421      *
073700050421     c                   ENDSR
073800050421      **-------------------------------------------------------------**
073900050421      ** Controllo dati inseriti in videata LV67W01                  **
074000050421      **-------------------------------------------------------------**
074100050421     c     sr_CtrW01     BEGSR
074200050421      *
074300050421      * P.O. Partenza
074400050421     c                   clear                   W1Dpop
074500050421      * - se *blanks lo considero *zeros (sarebbe numerico)
074600050421if  1c                   if        W1Cpop =  *blanks
074700050421     c                   eval      W1Cpop =  *zeros
074800050421e   1c                   endif
074900050421      * - ricerca
075000050421     c     '?'           scan      W1Cpop                                 30
075100050421if  1c                   if        *in30  =  *on
075200050421     c                   eval      W1Cpop =  *zeros
075300050421     c                   eval      Wcampo =  'W1CPOP    '
075400050421     c                   exsr      sr_Search
075500050421     c***                seton                                        48  90
075600050421e   1c                   endif
075700050421      * - controlli:
075800050421      * - - numericità
075900050421     c     DigitN        check     W1Cpop                                 30
076000050421if  1c                   if        *in30  =  *on
076100050421     c                   seton                                        482890
076200050421     c                   movel     Msg(3)        W1Cmsg
076300050421     c                   leavesr
076400050421e   1c                   endif
076500050421      * - - effettiva immissione
076600050421if  1c                   if        W1Cpop <= *zeros
076700050421     c                   seton                                        482890
076800050421     c                   movel     Msg(2)        W1Cmsg
076900050421     c                   leavesr
077000050421e   1c                   endif
077100050421      * - - validità
077200050421     c                   move      W1Cpop        Wpo
077300050421     c                   exsr      sr_CtrPO
077400050421     c                   movel     WpoD          W1Dpop
077500050421if  1c                   if        *in90  =  *on
077600050421     c                   seton                                        4828
077700050421     c                   movel     Msg(3)        W1Cmsg
077800050421     c                   leavesr
077900050421e   1c                   endif
078000050421      * - - non ammessi 995-998
078100050421if  1c                   if            Wpo >= £POX
078200050421     c                             and Wpo <  999
078300050421     c                   seton                                        482890
078400050421     c                   movel     Msg(16)       W1Cmsg
078500050421     c                   leavesr
078600050421e   1c                   endif
078700050421      *
078800050421      * P.O. Arrivo
078900050421     c                   clear                   W1Dpoa
079000050421      * - se *blanks lo considero *zeros (sarebbe numerico)
079100050421if  1c                   if        W1Cpoa =  *blanks
079200050421     c                   eval      W1Cpoa =  *zeros
079300050421e   1c                   endif
079400050421      * - ricerca
079500050421     c     '?'           scan      W1Cpoa                                 30
079600050421if  1c                   if        *in30  =  *on
079700050421     c                   eval      W1Cpoa =  *zeros
079800050421     c                   eval      Wcampo =  'W1CPOA    '
079900050421     c                   exsr      sr_Search
080000050421     c***                seton                                        49  90
080100050421e   1c                   endif
080200050421      * - controlli:
080300050421      * - - numericità
080400050421     c     DigitN        check     W1Cpoa                                 30
080500050421if  1c                   if        *in30  =  *on
080600050421     c                   seton                                        492890
080700050421     c                   movel     Msg(5)        W1Cmsg
080800050421     c                   leavesr
080900050421e   1c                   endif
081000050421      * - - effettiva immissione
081100050421if  1c                   if        W1Cpoa <= *zeros
081200050421     c                   seton                                        492890
081300050421     c                   movel     Msg(4)        W1Cmsg
081400050421     c                   leavesr
081500050421e   1c                   endif
081600050421      * - - validità
081700050421     c                   move      W1Cpoa        Wpo
081800050421     c                   exsr      sr_CtrPO
081900050421     c                   movel     WpoD          W1Dpoa
082000050421if  1c                   if        *in90  =  *on
082100050421     c                   seton                                        4928
082200050421     c                   movel     Msg(5)        W1Cmsg
082300050421     c                   leavesr
082400050421e   1c                   endif
082500050421      * - - non ammessi 995-998
082600050421if  1c                   if            Wpo >= £POX
082700050421     c                             and Wpo <  999
082800050421     c                   seton                                        492890
082900050421     c                   movel     Msg(16)       W1Cmsg
083000050421     c                   leavesr
083100050421e   1c                   endif
083200050421      *
083300050421      * Compatibilità P.O.
083400050421if  1c                   if            W1Cpop = '999'
083500050421     c                             and W1Cpoa = '999'
083600050421     c                   seton                                        482890
083700050421     c                   movel     Msg(17)       W1Cmsg
083800050421     c                   leavesr
083900050421e   1c                   endif
084000050421      *
084100050421      * Range zone
084200050427sel 1c                   select
084300050427w   1c                   when      W1Czoi > W1Czof
084400050421     c                   seton                                        502890
084500050421     c                   movel     Msg(18)       W1Cmsg
084600050421     c                   leavesr
084700050427w   1c                   when      W1Czoi = W1Czof
084800050427     c                   seton                                        502890
084900050427     c                   movel     Msg(19)       W1Cmsg
085000050427     c                   leavesr
085100050427e   1c                   endsl
085200050421      *
085300050421     c                   ENDSR
085400050421      **-------------------------------------------------------------**
085500050421      ** Aggiornamento SFL con nuovi record                          **
085600050421      **-------------------------------------------------------------**
085700050421     c     sr_AddSflRec  BEGSR
085800050421      *
085900050421      * Scorro il sfl per determinare la 1ª riga libera
086000050421     c                   eval      NRR = C_SflSiz
086100050421      *
086200050421do  1c                   dow       NRR > *zeros
086300050421      *
086400050421     c     NRR           chain     LV67S02                            32
086500050421      *
086600050421if  2c                   if        NOT *in32
086700050421      *
086800050421      * - Verifico se nella riga risultano inseriti dati
086900050421if  3c                   if            (V2Slnp <> *zeros
087000050421     c                             and  V2Slnp <> *blanks)
087100050421     c                             OR  (V2Slna <> *zeros
087200050421     c                             and  V2Slna <> *blanks)
087300050421     c                             OR   V2Czon <> *zeros
087400050421     c                             OR   V2Cbai <> *zeros
087500050421     c                   leave
087600050421e   3c                   endif
087700050421      *
087800050421e   2c                   endif
087900050421      *
088000050421     c                   sub       1             NRR
088100050421      *
088200050421e   1c                   enddo
088300050421      *
088400050421      * Aggiungo i records nel sfl (...finchè ce ne stanno)
088500050421     c                   eval      Wzona3  = W1Czoi
088600050421do  1c                   dow           NRR    >  *zeros
088700050421     c                             and NRR    <  C_SflSiz
088800050421     c                             and Wzona3 <= W1Czof
088900050421      *
089000050421     c                   eval      NRR = NRR + 1
089100050421     c     NRR           CHAIN     LV67S02
089200050421if  2c                   if        %found(FNLV67D)
089300050421     c                   clear                   LV67S02
089400050421     c                   eval      V2Slnp  = W1Cpop
089500050421     c                   movel     W1Dpop        V2Dlnp
089600050421     c                   eval      V2Slna  = W1Cpoa
089700050421     c                   movel     W1Dpoa        V2Dlna
089800050421     c                   eval      V2Czon  = Wzona3
089900050421     c                   add       1             Wzona3
090000050421     c                   eval      V2Cbai  = W1Cbai
090100050421     c                   UPDATE    LV67S02
090200050421e   2c                   endif
090300050421      *
090400050421e   1c                   enddo
090500050421      *
090600050421      * Segnalo?se?NON sono stati inseriti tutti i records nel sfl
090700050421      *  (1 rec. per zona)
090800050421if  1c                   if        Wzona3 <= W1Czof
090900050427     c                   movel     Msg(20)       VCCmsg
091000050422     c                   move      Wzona3        WzonaA
091100050422     c                   eval      VCCmsg = %trim(%replace(WzonaA:VCCmsg:
091200050422     c                                      %scan('&Z':VCCmsg)))
091300050422     c                   move      W1Czof        WzonaA
091400050422     c                   eval      VCCmsg = %trim(%replace(WzonaA:VCCmsg:
091500050422     c                                      %scan('&Z':VCCmsg)))
091600050421     c                   seton                                        412890
091700050421     c                   leavesr
091800050421e   1c                   endif
091900050421      *
092000050421     c                   ENDSR
092100050421      **-------------------------------------------------------------**
092200050421      ** Gestione ricerche                                           **
092300050421      **-------------------------------------------------------------**
092400050421     c     sr_Search     BEGSR
092500050421      *
092600050421sel 1c                   select
092700050421      *
092800050421      * P.O. (Partenza o Arrivo)
092900050421w   1c                   when          Wcampo =  'V2SLNP    '
093000050421     c                             or  Wcampo =  'V2SLNA    '
093100050421     c                             or  Wcampo =  'W1CPOP    '
093200050421     c                             or  Wcampo =  'W1CPOA    '
093300050421     c                   clear                   DsTNSD
093400050421     c                   call      'TNSD24R'
093500050421     c                   parm                    §cod
093600050421     c                   parm                    §tip
093700050421     c                   parm                    §des
093800050421     c                   parm                    §pra
093900050421sel 2c                   select
094000050421w   2c                   when      Wcampo = 'V2SLNP    '
094100050421     c                   movel     §cod          V2Slnp
094200050421     c                   movel     §des          V2Dlnp
094300050421w   2c                   when      Wcampo = 'V2SLNA    '
094400050421     c                   movel     §cod          V2Slna
094500050421     c                   movel     §des          V2Dlna
094600050421w   2c                   when      Wcampo = 'W1CPOP    '
094700050421     c                   movel     §cod          W1Cpop
094800050421     c                   movel     §des          W1Dpop
094900050421w   2c                   when      Wcampo = 'W1CPOA    '
095000050421     c                   movel     §cod          W1Cpoa
095100050421     c                   movel     §des          W1Dpoa
095200050421e   2c                   endsl
095300050421      *
095400050421e   1c                   endsl
095500050421      *
095600050421     c                   ENDSR
095700050421      **-------------------------------------------------------------**
095800050421      ** Controllo singolo P.O.                                      **
095900050421      **-------------------------------------------------------------**
096000050421     c     sr_CtrPO      BEGSR
096100050421      *
096200050421     c                   clear                   WpoD
096300050421      *
096400050421if  1c                   if        Wpo    <  £POX
096500050421      *
096600050421     c     Wpo           chain     AZORG
096700050421      *
096800050421if  2c                   if        NOT   %found(AZORG01L)
096900050421     c                             or    %found(AZORG01L)
097000050421     c                             and  ( ORGfva <> *blanks
097100050421     c                              or   (ORGfag <> 'A'
097200050421     c                              and   ORGfag <> 'F'))
097300050421     c                   eval      *in90  =  *on
097400050421x   2c                   else
097500050421if  3c                   if        ORGde5 <> *blanks
097600050421     c                   movel     ORGde5        WpoD
097700050421x   3c                   else
097800050421     c                   movel     ORGdes        WpoD
097900050421e   3c                   endif
098000050421e   2c                   endif
098100050421      *
098200050421x   1c                   else
098300050421      *
098400050421sel 2c                   select
098500050421w   2c                   when      Wpo = 999
098600050421     c                   movel(p)  'TUTTE'       WpoD
098700050421w   2c                   when      Wpo = 998
098800050421     c                   movel(p)  Tes(2)        WpoD
098900050421w   2c                   when      Wpo = 997
099000050421     c                   movel(p)  Tes(3)        WpoD
099100050421w   2c                   when      Wpo = 996
099200050421     c                   movel(p)  Tes(4)        WpoD
099300050421w   2c                   when      Wpo = 995
099400050421     c                   movel(p)  Tes(5)        WpoD
099500050421w   2c*** solo p.o.arr   when      Wpo = 990
099600050421     c***                movel(p)  Tes(6)        WpoD
099700050421e   2c                   endsl
099800050421      *
099900050421e   1c                   endif
100000050421      *
100100050421     c                   ENDSR
100200980224      *-------------------------------------------------------------------------
100300980224      * DUPLICAZIONE PIANO DI SMISTAMENTO IN UN ALTRO
100400980224      *-------------------------------------------------------------------------
100500980224     C     DUPTR         BEGSR
100600980224      *
100700020618     C                   MOVE      V1CPOG        V3CAOG
100800980224     C                   CLEAR                   V3CATL
100900980224     C                   CLEAR                   V3CAPL
101000020618     C                   MOVE      V1CPOG        V3CDOG
101100980224     C                   CLEAR                   V3CDTL
101200980224     C                   CLEAR                   V3CDPL
101300980224      *
101400980224    1C                   DO        *HIVAL
101500980224     C                   EXFMT     LV67D03
101600980224      *
101700980224      * pulizia campo messaggio e relativo indicatore  (*IN28)
101800980224     C                   CLEAR                   V3CMSG
101900980224     C                   SETOFF                                       28
102000980224      * azzero indicatori relativi ai messaggi di errore
102100980224     C                   SETOFF                                       4546
102200020618     C                   SETOFF                                       4849
102300980224      *
102400980224      ** Ritorno alla videata precedente F12
102500980224     C   KL              LEAVE
102600020618      *
102700020618      * F18=Cambio P.O. in gestione
102800020618     c   KS              eval      *in05 = *on
102900020618     c   KS              iter
103000020618      *
103100020618      * 05 on --> controllo P.O. da cui copiare
103200020618     c                   if        *in05
103300020618     c                   move      V3Cdog        WRKPOG
103400020618     c                   exsr      CTRFGS
103500020618     c   90              movel     *on           *in49
103600020618     c   90              movel     D50msg        V3Cmsg
103700020618     c   90              iter
103800020618     c                   eval      *in05 = *off
103900020618     c                   endif
104000980224      *
104100980224      ** Controllo piani di smistamento inseriti
104200980224      *  devono essere diversi da blanks
104300020618    2C     V3CAOG        IFEQ      *ZEROS
104400020618     C     V3CATL        OREQ      *BLANK
104500980224     C     V3CAPL        OREQ      *BLANK
104600980224     C                   MOVEL     MSG(10)       V3CMSG
104700980224     C                   SETON                                        4528
104800980224     C                   ITER
104900980224    2C                   ENDIF
105000980224      *
105100980224     C                   MOVEL     V3CAPL        WCTL
105200980224      * il primo carattere del piano di lavorazione deve essere uguale
105300980224      * al tipo lavorazione
105400980224    2C     WCTL          IFNE      V3CATL
105500980224     C                   MOVEL     MSG(1)        V3CMSG
105600980224     C                   SETON                                        4528
105700980224     C                   ITER
105800980224    2C                   ENDIF
105900980224      * Controllo se il piano di smistamento che deve essere creato esiste già
106000020618     C                   MOVE      V3CAOG        wPOG
106100980224     C                   MOVE      V3CATL        V1CCTL
106200980224     C                   MOVE      V3CAPL        V1CNPL
106300020618     C     K03TR350      CHAIN     FLTR3500                           33
106400980224    2C     *IN33         IFEQ      *OFF
106500980224     C                   SETON                                        4528
106600980224     C                   MOVEL     MSG(9)        V3CMSG
106700980224     C                   ITER
106800020618    2C                   ENDIF
106900980224      *  devono essere diversi da blanks
107000020618    2C     V3CDOG        IFEQ      *ZEROS
107100020618     C     V3CDTL        OREQ      *BLANK
107200980224     C     V3CDPL        OREQ      *BLANK
107300980224     C                   MOVEL     MSG(10)       V3CMSG
107400980224     C                   SETON                                        4628
107500980224     C                   ITER
107600980224    2C                   ENDIF
107700980224      *
107800980224     C                   MOVEL     V3CDPL        WCTL
107900980224      * il primo carattere del piano di lavorazione deve essere uguale
108000980224      * al tipo lavorazione
108100980224    2C     WCTL          IFNE      V3CDTL
108200980224     C                   MOVEL     MSG(1)        V3CMSG
108300980224     C                   SETON                                        4628
108400980224     C                   ITER
108500980224    2C                   ENDIF
108600980224      * Controllo se il piano di smistamento da duplicare non  esiste
108700020618     C                   MOVE      V3CDOG        wPOG
108800980224     C                   MOVE      V3CDTL        V1CCTL
108900980224     C                   MOVE      V3CDPL        V1CNPL
109000020618     C     K03TR350      SETLL     FLTR3500                               33
109100980224    2C     *IN33         IFEQ      *OFF
109200980224     C                   SETON                                        4628
109300980224     C                   MOVEL     MSG(11)       V3CMSG
109400980224     C                   ITER
109500020618    2C                   ENDIF
109600020618      ** Duplicazione Dati  F6
109700980224    2C     *INKF         IFEQ      *ON
109800980224    3C                   DO        *HIVAL
109900020618     C     K03TR350      READE     FLTR3500
110000020618    4C                   IF        not %eof(FLTR351L)
110100020618     C                   MOVE      V3CAOG        TR3POG
110200980224     C                   MOVE      V3CATL        TR3CTL
110300980224     C                   MOVE      V3CAPL        TR3NPL
110400020618     C                   WRITE     FLTR3500
110500020618     C                   else
110600020618     C                   leave
110700020618    4C                   ENDIF
110800020618    3C                   ENDDO
110900980224      * Ricarico il subfile con il nuovo piano di smistamento
111000020618     C                   MOVE      V3CAOG        V1CPOG
111100980224     C                   MOVE      V3CATL        V1CCTL
111200980224     C                   MOVE      V3CAPL        V1CNPL
111300980224     C                   EXSR      CARSF1
111400980224     C                   Z-ADD     1             REC
111500980224     C                   SETON                                        2021
111600980224     C                   WRITE     LV67Z02
111700980224      *
111800980224     C                   LEAVE
111900020618    2C                   ENDIF
112000980224      **
112100980224    1C                   ENDDO
112200980224      *
112300980224     C                   ENDSR
112400020724      *---------------------------------------------------------------
112500020724      * POSIZIONAMENTO CURSORE AL 1° RECORD DELLA PAGINA
112600020724      *---------------------------------------------------------------
112700020724     C     POSCUR        BEGSR
112800020724      *
112900020724     C                   IF        V2CPSZ > 0
113000020724     C                   Z-ADD     V2CPSZ        REC
113100020724     C                   ENDIF
113200020724      *
113300020724     C     REC           DIV       SFLPAG        PAGINE            4 0
113400020724     C                   MVR                     RESTO             3 0
113500020724     C     PAGINE        MULT      SFLPAG        REC
113600020724     C     RESTO         IFGT      0
113700020724     C                   ADD       1             REC
113800020724     C                   ELSE
113900020724     C                   SUB       SFLPAG        REC
114000020724     C                   ADD       1             REC
114100020724     C                   ENDIF
114200020724      *
114300020724     C                   ENDSR
114400980224** TESTATA PROGRAMMA
114500980224MANUTENZIONE  PIANI DI SMISTAMENTO
114600980227NON LETTI
114700000221NON QUADRATI
114800000719NON STAMPATI
114900011210INVERSIONI
115000040924FORZ.INSTRAD.
115100980223** MESSAGGI ERRORI
115200980224Primo carattere del piano lavorazione deve essere uguale al tipo lavorazione   1
115300980224Inserire la linea di partenza                                                  2
115400980224Linea di partenza inesistente                                                  3
115500980224Inserire la linea di arrivo                                                    4
115600980224Linea di arrivo inesistente                                                    5
115700980224Zona errata                                                                    6
115800980224Inserire la baia                                                               7
115900980224Baia errata                                                                    8
116000980224Piano di smistamento già esistente impossibile crearlo                         9
116100980224Inserimento obbligatorio dei parametri di duplicazione                        10
116200980224Piano di smistamento non esiste impossibile duplicarlo                        11
116300011210Inserire il tipo lavorazione                                                  12
116400011210Manca la baia                                                                 13
116500020724Solo ammessi solo valori numerici                                             14
116600050420Inserito carattere non valido: ' (apice)                                      15
116700050421Per questa funzione NON sono ammessi P.O. dal 995 al 998 (riservati)          16
116800050421Impossibile immettere entrambi i P.O. (di partenza e di arrivo) "999"         17
116900050421Zona iniziale > Zona finale                                                   18
117000050427Immettere più di una zona                                                     19
117100050427Spazio insufficiente per i record "automatici" dalla zona &Z alla zona &Z     20
117200080526Utente NON abilitato. Premere Invio per uscire dal programma.                 21
