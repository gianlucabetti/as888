000100990507     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PJXBND PJCBND)
000200020412     H*PARMS ACTGRP(YFICN51)
000300021203     Ffiapd01L  IF   E           K DISK
000400031119     Ftntbe01L  IF   E           K DISK
000500031119     Fansog01l  IF   E           K DISK
000600031119     Fanfrn01l  IF   E           K DISK
000700031119     Fanrco01l  IF   E           K DISK
000800020415     F*--------
000900031119     Fficn93D   CF   E             WORKSTN
001000980911     F                                     SFILE(y350S01:NRR)
001100951009     D*---------------------------------------------------------------*
001200020411     D* Reperimento nome PGM
001300020411     D STATUS         SDS           333
001400020411     D  nompgm           *PROC
001500020411     D*---------------------------------------------------------------*
001600020411     D* CAMPI INTERNI
001700020411     D*---------------------------------------------------------------*
001800020411     D dataiso         s               d   datfmt(*iso)
001900020411     D dataeur         s               d   datfmt(*eur)
002000031119     D ric             s              1  0 inz(1)
002100951009?     *--------------------------------------------------------------*
002200951009?     *  DS                                                          *
002300951009?     *--------------------------------------------------------------*
002400951009     D KPJBA         E DS
002500100729     D ddatiute      e ds
002600100729     D azuteds       e ds                  extname(AZUTE00F)
002700100729     D tibs34ds      E DS                  inz
002800031120     d* parametri passati
002900031120     D param           DS
003000031120     d parrrn                         9s 0
003100031120     d opz                            2
003200031120     d cmd                            2
003300031120     d* parametri ricevuti
003400031119     D ficn92ds      E DS
003500031119     D dcaf          E DS
003600031119     d*
003700031119     D fiott           DS
003800031119     d pdrs                           7  0
003900031119     d ddcs                           8  0
004000031119     d caus                           4
004100031119     d imps                          10  3
004200031119     d tips                           1
004300031119     d socs                           3
004400031119     d fors                           8
004500031125     d dtfs                           8  0
004600031125     d nffs                           9  0
004700031125     d nras                           9  0
004800031119     d rrns                           9  0
004900951009?     *--------------------------------------------------------------*
005000951009?     *  CICLO PRINCIPALE                                            *
005100951009?     *--------------------------------------------------------------*
005200031119     c                   do        *hival
005300031119     c                   if        ric = 1
005400951010     C                   EXSR      CARSFL
005500031119     c                   end
005600031119     C                   exsr      GESS01                                       sfl pieno
005700031119     C                   ENDdo
005800980312     C*
005900951009?     *--------------------------------------------------------------*
006000951012?     *  CARSFL: CARICAMENNTO SUBFILE                                *
006100951009?     *--------------------------------------------------------------*
006200951012     C     CARSFL        BEGSR
006300951009      *
006400951009      *  Pulisco dati SFL
006500020405     C                   SETOFF                                       2120
006600980911     C                   WRITE     y350C01
006700951009     C                   SETON                                        2021
006800020415     C                   Z-ADD     0             NRR               6 0
006900031119     C/EXEC SQL
007000031119     C+ DECLARE A1 CURSOR FOR SELECT OTTPDR, OTTDDC, OTTTAB, OTTTIM,
007100031125     C+ OTTTIP, OTTSOC, OTTCDF, OTTDFT, OTTNFF, OTTNRA, rrn(fiott00f)
007200031125     C+ FROM fiott00f WHERE ottpdr between :f92pd1 and :f92pd2 and ottcdf
007300031125     C+ between :f92fo1 and :f92fo2 and ottddc between :f92dti and
007400031125     C+ :f92dtf ORDER BY OTTTAB, OTTTIP, OTTPDR, OTTDDC
007500031119     C/END-EXEC
007600031119     c* Lettura fiott00f
007700031119
007800020405     C/EXEC SQL
007900020405     C+ OPEN A1
008000020405     C/END-EXEC
008100020405
008200020405     C                   DOU       SqlCod <> 0
008300030827     C/EXEC SQL
008400031119     C+ FETCH NEXT FROM A1 INTO :fiott
008500030827     C/END-EXEC
008600020405     c*
008700020405     C                   SELECT
008800020405     C                   WHEN      SqlCod = 0
008900031119     C* controlli record letto
009000031119     c                   if        f92soc<> ' ' and socs <> f92soc
009100031119     c                   iter
009200031119     c                   end
009300031119     c                   if        f92tip<> ' ' and tips <> f92tip
009400031119     c                   iter
009500031119     c                   end
009600031119     c                   if        f92tab<> ' ' and caus <> f92tab
009700031119     c                   iter
009800031119     c                   end
009900040107     c* se caricati 9000 record di sfl metto un messaggio di parzializzare
010000040107     c                   if        nrr = 9000
010100040107     c                   seton                                        23
010200040107     c                   eval      $msg = 'Raggiunto massimo visualizzabile. -
010300040107     c                             F12 e parzializzare.'
010400040107     c                   leave
010500040107     c                   end
010600031119     c*
010700951010     C                   EXSR      CARTRN
010800020405     C                   WHEN      SqlCod = 100
010900020405     C                   WHEN      SqlCod <> 0
011000020417      * Forzo la stampa del JOBLOG.
011100020417     C                   CALL      'X66CHGJOB'
011200020405     C                   ENDSL
011300020404     C                   ENDDO
011400020417     C/EXEC SQL
011500020417     C+ CLOSE A1
011600020417     C/END-EXEC
011700031121     c                   clear                   ric
011800951009     C*
011900020415     C                   ENDSR
012000951010?     *--------------------------------------------------------------*
012100020405?     *  CARTRN: Gestione caricamento dati                           *
012200951010?     *--------------------------------------------------------------*
012300951010     C     CARTRN        BEGSR
012400951010      *
012500031121     c                   movel     *blanks       v1csce
012600031119     c                   MOVE      tips          V1Ctip
012700031119     c                   MOVE      pdrs          V1Cpdr
012800031119     C                   Z-ADD     rrns          h1rrn
012900031119     C                   Z-ADD     nras          h1nra
013000031119     C                   Z-ADD     imps          v1cimp
013100031119     c                   move      ddcs          dataiso
013200030827     c                   move      dataiso       dataeur
013300031119     c                   move      dataeur       v1cdta
013400031125     C                   IF        dtfs <> 0
013500031125     c                   move      dtfs          dataiso
013600031125     c                   move      dataiso       dataeur
013700031125     c                   move      dataeur       v1cdft
013800031125     c                   move      nffs          v1cnff
013900031125     c                   else
014000031125     c                   clear                   v1cnff
014100031125     c                   clear                   v1cdft
014200031125     c                   end
014300030827     C     kapd          CHAIN     fiapd01L                           30
014400030827     C                   MOVEL     apdrsf        v1dpdr
014500031119     C                   MOVEL(p)  caus          tbeke1
014600031119     C     ktbe          CHAIN     tntbe01L                           30
014700031119     C                   MOVEL     tbeuni        dcaf
014800031119     C                   MOVEL     §cafdes       v1dtab
014900031119     c                   MOVEl     socs          V1Csoc
015000031119     c                   MOVEl     fors          V1Cfor
015100031119     c                   movel     *blank        sogdes
015200031119     C     Kfrn          CHAIN     anfrn01l                           30
015300031119     C  N30Krco          CHAIN     anrco01l                           30
015400031119     C  N30rcosogg       CHAIN     ansog01l                           30
015500031119     C                   MOVEL(p)  sogdes        v1dfor
015600020408     c*
015700020405     C                   ADD       1             NRR
015800020405     C                   WRITE     y350S01
015900951010      *
016000951010     C                   ENDSR
016100020405?     *--------------------------------------------------------------*
016200020405?     *  GESS01: Gestione prima videata                              *
016300020405?     *--------------------------------------------------------------*
016400020405     C     GESS01        BEGSR
016500020405      *
016600031120     c                   if        nrr = 0
016700031120     c                   exfmt     y350D01
016800031120     c                   else
016900980914     C                   WRITE     y350T01
017000980914     C                   EXFMT     y350C01
017100031120     c                   end
017200020405     c* gestione comandi
017300020405     c                   exsr      srcmd
017400031119     c* gestione opzioni
017500031120     c                   if        nrr > 0
017600031119     c                   exsr      sropz
017700031120     c                   end
017800951009      *
017900020417     C                   ENDSR
018000031119?     *--------------------------------------------------------------*
018100031119?     *  srcmd   Gestione comandi                                    *
018200031119?     *--------------------------------------------------------------*
018300031119     C     srcmd         BEGSR
018400031119      *  Fine Lavoro
018500031119      *  Ritorno
018600031119     C     *INKC         IFEQ      '1'
018700031119     C     *INKL         orEQ      '1'
018800031119     C                   exsr      fine
018900031119     C                   END
019000031119      *  Inserimento
019100031119     C     *INKj         IFEQ      '1'
019200031120     c                   clear                   h1rrn
019300031119     c                   exsr      srcall
019400031119     C                   END
019500031119      *
019600031119     C                   ENDSR
019700031119?     *--------------------------------------------------------------*
019800031119?     *  sropz: Controlli prima videata                              *
019900031119?     *--------------------------------------------------------------*
020000031119     C     sropz         BEGSR
020100031119      *
020200031119     c                   clear                   $msg
020300031119      *  Solo i selezionati
020400031120     c                   do        *hival
020500031119     C                   READC     y350S01                                29
020600031120     C                   if        *in29
020700031120     c                   leave
020800031120     c                   end
020900031121     c                   setoff                                       23
021000031120     C                   if        v1csce = '4' or v1csce = '3'
021100031121     C                             or v1csce = '5' or v1csce = '2'
021200031121     c* posso fare l'opzione 2 o 4 solo se non ho ancora contabilizzato
021300031120     c* invece l'opzione 3 e 5 sono sempre possibile
021400031121     C                   if        h1nra<>0 and (v1csce='2' or v1csce='4')
021500031121     c                   seton                                        23
021600031121     c                   eval      $msg = 'Opzione non ammessa: riga già -
021700031121     c                             contabilizzata'
021800031120     c                   else
021900031120     c                   exsr      srcall
022000031119     c                   end
022100031121     c                   end
022200031121     c                   movel     *blanks       v1csce
022300031121     c                   if        *in23
022400031121     c                   seton                                        22
022500031121     c                   else
022600031121     C                   SETOFF                                       22
022700031121     c                   end
022800031119     C                   UPDATE    y350S01
022900031121     c   23              leave
023000031119     c                   enddo
023100031119      *
023200031119     C                   ENDSR
023300951011?     *--------------------------------------------------------------*
023400951011?     *  *INZSR: Operazioni di inizializzazione dati                 *
023500951011?     *--------------------------------------------------------------*
023600951011     C     *INZSR        BEGSR
023700951011      *
023800951011     C     *ENTRY        PLIST
023900951011     C                   PARM                    KPJBA
024000031119     C                   MOVEL     KPJBU         ficn92ds
024100100729     c     *dtaara       define    §azute        azuteds
024200100729     c     *dtaara       define    §datiute      ddatiute
024300100729     C                   in(E)     *dtaara
024400100729     C                   IF        %Error  or  RSUT = *blanks
024500100729     C                   call      'TIBS34R'
024600100729     C                   parm                    Tibs34Ds
024700100729     C                   in        *dtaara
024800100729     c                   ENDIF
024900100729     C                   movel     '000400'      forita            6
025000031119     C     Kfrn          KLIST
025100031119     C                   KFLD                    v1csoc
025200031119     C                   KFLD                    v1cfor
025300031119     C     Krco          KLIST
025400031119     C                   KFLD                    v1csoc
025500031119     C                   KFLD                    forita
025600031119     C                   KFLD                    v1cfor
025700021203     c     kapd          klist
025800031119     c                   kfld                    v1ctip
025900030827     c                   kfld                    v1cpdr
026000031119     c     ktbe          klist
026100031119     c                   kfld                    tbecod
026200031119     c                   kfld                    tbeke1
026300031119     c                   eval      tbecod = 'CAF'
026400951012     C                   ENDSR
026500031119     C*----------------------------------------------------*
026600031119     C     fine          BEGSR
026700031119     C*----------------------------------------------------*
026800031120     c                   clear                   f92cmd
026900031119     C     *INKL         IFEQ      '1'
027000031119     C                   MOVEL     '12'          f92CMD
027100031119     C                   END
027200031120     C     *INKC         IFEQ      '1'
027300031119     C                   MOVEL     '03'          f92CMD
027400031119     C                   END
027500031119     C                   MOVEL(P)  ficn92ds      KPJBU
027600031119     c                   seton                                        lr
027700031119     c                   return
027800031119     C                   ENDSR
027900031119     C*----------------------------------------------------*
028000031119     C     srcall        BEGSR
028100031119     C*----------------------------------------------------*
028200031119     c                   clear                   ric
028300031120     c                   eval      parrrn = h1rrn
028400031120     c                   move      *all'0'       opz
028500031120     c                   if        not *inkj
028600031120     c                   move      v1csce        opz
028700031120     c                   else
028800031120     c                   eval      opz ='06'
028900031120     c                   end
029000031120     c                   clear                   cmd
029100031120     c                   movel(p)  param         kpjbu
029200031119     C                   call      'FICN94R'
029300031119     c                   parm                    kpjba
029400031120     c                   movel     kpjbu         param
029500031120     c*
029600031120     c                   select
029700031120     c* conferma allora ricarico x aggionamenti
029800031120     c                   when      cmd ='06'
029900031119     c                   eval      ric = 1
030000031120     c                   when      cmd ='03'
030100031120     c                   move      '1'           *inkc
030200031120     c                   exsr      fine
030300031120     c                   endsl
030400031119     C                   ENDSR
