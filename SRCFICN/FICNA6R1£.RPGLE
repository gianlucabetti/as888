000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400020305
000500020305     h* OVRPRTF FILE(QSYSPRT) LPI(6) CPI(15) PAGRTT(0)
000600020305
000700081003     Ffiadt01l  uf   e           k disk
000800081003     Ffiadd01l  if   e           k disk
000900081003     Ffiapd01l  if   e           k disk
001000060320     Fanfrn01l  if   e           k disk
001100080918     Fancdp01l  if   e           k disk
001200060321     Fansog01l  if   e           k disk
001300060320     Fazorg01l  if   e           k disk
001400101111     F*****ansif01l  if   e           k disk
001500020308     Fqsysprt   O    F  132        PRINTER prtctl(prtds) usropn
001600011026
001700020304     dkpjba          e ds
001800020304
001900020304     dparam            ds
002000081003     D  parpdr                        7s 0
002100081003     D  parprg                        3s 0
002200081003     D  partip                        1
002300020304
002400020402     d modo            s              8
002500020114     d base            s              1
002600090114     d costt           s             33
002700060321     d costa           s             74
002800090324     d costa2          s             15
002900060321     d costa2b         s             16
003000090324     d costa3          s             50
003100020305     d dataiso         s               d   datfmt(*iso)
003200020305     d dataeur         s               d   datfmt(*eur)
003300020117     d prtds           ds
003400020117     d   spab                         3  0
003500020117     d   spaa                         3  0
003600020117     d   skab                         3  0
003700020117     d   skaa                         3  0
003800020117     d   line                         3  0
003900081007     d prtds1          ds
004000081007     d   spab1                        3  0
004100081007     d   spaa1                        3  0
004200081007     d   skab1                        3  0
004300081007     d   skaa1                        3  0
004400081007     d   line1                        3  0
004500090324     D CMD             S             80    DIM(2) CTDATA PERRCD(1)              OVRPRTF
004600080916     d locemi          s             40
004700080916     D DVCOBADS      E DS
004800080916     D                                     INZ
004900090114     d ficn05ds      e ds
005000080916     ddgpi           e ds
005100101111     ***************************************************************************
005200101111     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
005300101111     D ESITO_OK...
005400101111     D                 C                   0
005500101111     ***************************************************************************
005600101111     **
005700101111     ** Dichiarazione prototipi procedure esterne.
005800101111     **
005900101111     ***************************************************************************
006000101111      /DEFINE DFTACTGRP_YES
006100101111     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
006200101111      /UNDEFINE DFTACTGRP_YES
006300101111     ***************************************************************************
006400101111     **
006500101111     ** Definizione strutture dati.
006600101111     **
006700101111      **************************************************************************
006800101111     D tibsSocI0...
006900101111     D               E DS                  QUALIFIED
007000101111     D                                     INZ
007100101111     D tibsSocO0...
007200101111     D               E DS                  QUALIFIED
007300101111     D                                     INZ
007400101111     ***************************************************************************
007500101111     **
007600101111     ** Definizione variabili modulo/programma.
007700101111     **
007800101111     ***************************************************************************
007900101111     D prmRqsOpCode...
008000101111     D                 S             10A
008100101111     D prmRpyOpCode...
008200101111     D                 S             10A
008300101111     D prmRpyIdMsg...
008400101111     D                 S             10I 0
008500101111     D prmRqsFormato...
008600101111     D                 S             10A
008700101111     D prmRqsDataSize...
008800101111     D                 S             10I 0
008900101111     D prmRpyFormato...
009000101111     D                 S             10A
009100101111     D prmRpyDataSize...
009200101111     D                 S             10I 0
009300101111
009400000000     C*---------------------------------------------------------------*
009500020308     c
009600081003      * decodifico fornitore
009700081003     C                   clear                   ragsvet
009800081003     c                   movel     *all'_'       cdpdescr
009900081003     C     kfrn          chain     anfrn01l
010000081003     c                   if        %found(anfrn01l)
010100081003     C     kcdp          chain     ancdp01l
010200081003     C     frnsogg       chain     ansog01l
010300081003     c                   if        %found(ansog01l)
010400081003     C                   MOVEL     sogdes        ragsvet          44
010500081003     c                   endif
010600081003     c                   endif
010700081003      *stampa testata allegato
010800081003     c                   except    testata
010900081003     c                   except    testo
011000081003      *
011100081003     c                   exsr      elabora
011200081003      *
011300020308     C* ESEGUO L'OVERRIDE DEL FILE DI STAMPA
011400020308     C                   close     qsysprt
011500020308     C                   MOVEL(p)  CMD(2)        COMMAN           80
011600020308     C                   Z-ADD     80            LUNG             15 5
011700020308     C                   CALL      'QCMDEXC'
011800020308     C                   PARM                    COMMAN
011900020308     C                   PARM                    LUNG
012000081007
012100101111     ** Chiudo il programma.
012200101111     C                   CALL      'TIBSSOCR'
012300101111     C                   PARM      'FINALIZE'    prmRqsOpCode
012400101111     C                   PARM                    prmRpyOpCode
012500101111     C                   PARM                    prmRpyIdMsg
012600101111     C*
012700020308     c                   seton                                        lr
012800020308      *--------------------------------------------------
012900020308     c     elabora       begsr
013000020308      *--------------------------------------------------
013100020411     c
013200081003     c     kadt          setll     fiadd01l
013300081003     c                   do        *hival
013400081003     c     kadt          reade     fiadd01l
013500081003      *fine file
013600081003     c                   if        %eof(fiadd01l)
013700081003     c                   leave
013800081003     c                   endif
013900081003     c                   if        line > 58
014000081003     c                   except    testata
014100081003     c                   endif
014200081003      *decodifica partenza
014300081003     c     addlnp        chain     azorg01l
014400081003     c                   if        %found(azorg01l)
014500081003     c                   movel     orgdes        partenza         20
014600081003     c                   else
014700081003     c                   clear                   partenza
014800081003     c                   endif
014900081003      *decodifica arrivo
015000081003     c     addlna        chain     azorg01l
015100081003     c                   if        %found(azorg01l)
015200081003     c                   movel     orgdes        arrivo           20
015300081003     c                   else
015400081003     c                   clear                   arrivo
015500081003     c                   endif
015600081003      * stampa riga listino attiva
015700081003     c                   except    dettaglio
015800081003     c                   enddo
015900060320
016000020308     c                   endsr
016100020304
016200011026     c*-------------------------------------------------
016300011026     c     *inzsr        begsr
016400011026     c*-------------------------------------------------
016500020304     c     *entry        plist
016600020304     c                   parm                    kpjba
016700020304     c                   movel     kpjbu         param
016800020304
016900081003     c     kapd          klist
017000081003     c                   kfld                    tipoa             1
017100081003     c                   kfld                    parpdr
017200081003     c                   move      'D'           tipoa
017300081003     c     kadt          klist
017400081003     c                   kfld                    parpdr
017500081003     c                   kfld                    parprg
017600081003     c
017700060320     c     kfrn          klist
017800060320     c                   kfld                    socie
017900060320     c                   kfld                    fornit            8
018000060320     c                   move      *all'0'       fornit
018100080918     c     kcdp          klist
018200081003     c                   kfld                    socie             3
018300080918     c                   kfld                    frncondpag
018400101111     C*
018500101111     ** Inizializzo il programma.
018600101111     C                   CALL      'TIBSSOCR'
018700101111     C                   PARM      'INIT'        prmRqsOpCode
018800101111     C                   PARM                    prmRpyOpCode
018900101111     C                   PARM                    prmRpyIdMsg
019000021203
019100020218     C                   time                    w0120            14 0
019200020218     C                   move      w0120         wdat              8 0
019300090114      * data da impostare su data stampa e convalida se lancio = 'C'
019400020308     c                   move      wdat          dataeur
019500020308     c                   move      dataeur       dataiso
019600020308     c                   move      dataiso       dataoggi          8 0
019700081003     c* aggancio testata
019800081003     c     kadt          chain     fiadt01l
019900081003     c                   if        %found(fiadt01l)
020000090116     c                   if        partip = 'C' and adtdts = 0
020100081003     c                   move      dataoggi      adtdts
020200081003     c                   update    fiadt000
020300081003     c                   else
020400081003     c                   except    noagg
020500081003     c                   endif
020600081003     c                   endif
020700081003      * data decorrenza da stampare
020800081003     c                   move      adtddt        dataiso
020900081003     c                   move      dataiso       dataeur
021000081003     c                   move      dataeur       alladata          8 0
021100081003     c* aggancio anagrafica autisti per reperire codice fornitore
021200081003     c     kapd          chain     fiapd01l
021300081003     c                   if        %found(fiapd01l)
021400081003     c                   move      apdksc        fornit
021500081003     c                   move      apdcsf        socie
021600081003     c                   endif
021700011026
021800020117     c                   z-add     99            line
021900081007     c                   z-add     99            line1
022000020305     c                   move      *all'_'       separa          100
022100060321      * ragione sociale società mittente
022200101111     c**** socie         chain     ansif01l
022300101111     c********           if        %found(ansif01l)
022400101111     c********           movel     sifrgs        ragsmit          30
022500101111     c********           endif
022600101111      *
022700101111     c                   clear                   ragsmit          30
022800101111     c                   exsr      decod_societa
022900101111      *
023000080916     c* prezzo iniziale carburante legge 133
023100090114     c                   clear                   ficn05ds
023200090114     c                   call      'FICN05R'
023300090114     c                   parm                    ficn05ds
023400090114     c                   eval      dgpi = o05gpi
023500090114     c                   z-add     D§GPIGPI      prezzo            5 3
023600020304
023700090324     c                   eval      costt = 'STAMPA TARIFFA con decorrenza'
023800020412     c
023900020411     c*costanti pretariffa
024000090324     c                   eval      costa2= 'Fornitore '
024100060321     c                   eval      costa2b= 'codice fornitore'
024200090324     c                   eval      costa3= 'relativo alle prestazioni dell" -
024300081212     c                             autotrasportatore'
024400081212     c
024500020308     C* ESEGUO L'OVERRIDE DEL FILE DI STAMPA
024600020308     C                   MOVEL(p)  CMD(1)        COMMAN           80
024700020308     C                   Z-ADD     80            LUNG             15 5
024800020308     C                   CALL      'QCMDEXC'
024900020308     C                   PARM                    COMMAN
025000020308     C                   PARM                    LUNG
025100020308     C                   OPEN      qsysprt
025200081007      *
025300011026     c                   endsr
025400101111     C**************************************************************************
025500101111     C* decodifica la società
025600101111     C************************************************************
025700101111     C     Decod_societa begSR
025800101111      *
025900101111     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
026000101111     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
026100101111     C********           RESET                   tibsSocI0
026200101111     C                   EVAL      tibsSocI0.idSocieta = socie
026300101111     C
026400101111     C                   CALL      'TIBSSOCR'
026500101111     C                   PARM      'GETANAGRAF'  prmRqsOpCode
026600101111     C                   PARM      *BLANK        prmRpyOpCode
026700101111     C                   PARM      *ZERO         prmRpyIdMsg
026800101111     C                   PARM      'TIBSSOCI0'   prmRqsFormato
026900101111     C                   PARM                    tibsSocI0
027000101111     C                   PARM                    prmRqsDataSize
027100101111     C                   PARM      'TIBSSOCO0'   prmRpyFormato
027200101111     C                   PARM                    tibsSocO0
027300101111     C                   PARM                    prmRpyDataSize
027400101111     c                   if         PRMRPYIDMSG >= 0
027500101111     c                              and TIBSSOCO0.IDSOCIETA <> *blank
027600101111     c                   eval       ragsmit  = tibsSocO0.RAGSOCIALE
027700101111     c                   end
027800101111      *
027900101111     C                   ENDSR
028000011129     C*--------------------------------------------------------------------
028100081003     Ofiadt000  E            noagg
028200081003      *____________________________________________________________
028300081003     Oqsysprt   E            testata          06
028400090114     o                       costt               33
028500020412     o                       alladata          +  2 '  /  /    '
028600090324     o                                         +  2 'Progressivo'
028700090324     o                       parprg            +  2
028800020411     o                                          108 'Pag.'
028900020117     O                       page          z    115
029000020411     O          E            testata     0
029100090114     o                       costt               33
029200020412     o                       alladata          +  2 '  /  /    '
029300060322     O          E            testata     0
029400090114     o                       costt               33
029500020412     o                       alladata          +  2 '  /  /    '
029600020411      *testo previsto per collegamento al contratto
029700020411     O          E            testo       1
029800090324     o                       costa2              15
029900090324     o                       ragsvet             60
030000090324     o                       costa2b             80
030100090324     o                       frnksc              90
030200020411     O          E            testo       1
030300090324     o                       costa3              50
030400090507     o                       apdrsf            +  2
030500090324     o                                         +  2 'Codice autista'
030600090324     o                       apdpdr            +  2
030700020402      *
030800060320     O          E            testo       1
030900020305     o                       separa             100
031000060320     O          E            testo       2
031100081007     o                                           11 'Partenza'
031200081007     o                                           33 'Arrivo'
031300090113     o                                           55 'Importo'
031400090113     o                                           63 'Km.'
031500081007     o                                           70 'Note'
031600060320     O          E            testo       0
031700081007     o                                           11 'Partenza'
031800081007     o                                           33 'Arrivo'
031900090113     o                                           55 'Importo'
032000090113     o                                           63 'Km.'
032100081007     o                                           70 'Note'
032200060320     O          E            testo       0
032300081007     o                                           11 'Partenza'
032400081007     o                                           33 'Arrivo'
032500090113     o                                           55 'Importo'
032600090113     o                                           63 'Km.'
032700081007     o                                           70 'Note'
032800060320      *dettaglio
032900060320     O          E            dettaglio   1
033000081007     o                       partenza            20
033100081007     o                       arrivo              42
033200090113     o                       addimp        2     55
033300090113     o                       addkm         2     63
033400081007     o                       addnote            116
033500060320     O          E            dettaglio   0
033600081007     o                       partenza            20
033700081007     o                       arrivo              42
033800090113     o                       addimp        2     55
033900090113     o                       addkm         2     63
034000081007     o                       addnote            116
034100060324     O          E            dettaglio   0
034200081007     o                       partenza            20
034300081007     o                       arrivo              42
034400090113     o                       addimp        2     55
034500090113     o                       addkm         2     63
034600081007     o                       addnote            116
034700020313**
034800020308OVRPRTF FILE(QSYSPRT) LPI(6) CPI(15) PAGRTT(0)
034900020308DLTOVR FILE(QSYSPRT)
