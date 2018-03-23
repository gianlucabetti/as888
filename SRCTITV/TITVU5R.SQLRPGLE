000100060803      /TITLE Upload via Internet: traduzione in FIARG00F
000200121218     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600060906     FFIARG01L  uF A E           K DISK    prefix(u_)
000700060906     FFnarb01l  iF   E           K DISK
000800060906     FFilte01l  uF   E           K DISK
000900010330     D*----------------------------------------------------
001000070124     D* DICHIARAZIONE VARIABILI DI WRK
001100010330     D*----------------------------------------------------
001200070124
001300010330     D psds           sds
001400010330     D  procname         *PROC
001500010330     D tivlrds       e ds                  extname(tivlr00f)
001600060804     D dtllsi        e ds
001700060807     D fiargds       e ds                  extname(fiarg00f)
001800060807     D fiargds_u     e ds                  extname(fiarg00f) prefix(u_)
001900010330     D esito           s              1
002000060804     D flgTest         s              1    inz
002100010330     D prmlit          s             10
002200010330     D prmfir          s             10
002300010330     D wrkesito        s                   like(esito)
002400010330     D wrkdata         s               d
002500010330     D wrkora          s               t
002600070125     d Dataiso         s               d   datfmt(*iso)
002700070125     d Dataeur         s               d   datfmt(*eur)
002800010330     D rrnum           s              6  0 INZ(*zeros)
002900011113     D depcmd          s            150    INZ(*blanks)
003000060905     d
003100060906     D wIDB            s                   like(argIDB)
003200060906     D keyidb          s                   like(argIDB)
003300060906     D savlna          s                   like(arblna) inz
003400060905     D contaNSPG       s                   like(lteNSPG)
003500060905     D contaNSPEB      s                   like(lteNSPG)
003600060905     D contaNSPET      s                   like(lteNSPG)
003700060905     D contaNSPEW      s                   like(lteNSPG)
003800060905     D contaNSPR       s                   like(lteNSPG)
003900060906     D SAVtllidb       s                   like(§tllsiidb)
004000060906     d                                     inz('ccllffo')
004100060907     D ErrIDB          s              1    inz
004200110701     D fnlvstds      E DS                  inz
004300111018     D fidg51ds      E DS                  inz
004400110701     D kpjba         E DS                  inz
004500010330     D*------------------
004600010330     D* DS "XSRDA8" - CONTROLLA DATA (8)
004700010330     D*------------------
004800080116     D** WLBDA8          DS                  INZ
004900080116     D**  G08DAT                 1      8  0
005000080116     D**  G08INV                 9     16  0
005100080116     D**  G08ERR                17     17
005200080116     D**  G08TGI                18     22  0
005300000313     D*
005400060906     D*------------------
005500080109     C*
005600080109     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
005700080109     C
005800080109     C/EXEC SQL
005900080109     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
006000080109     C/END-EXEC
006100990915     C                   time                    wrkdata
006200990915     C                   time                    wrkora
006300000913     C                   reset                   rrnum
006400990921     C                   reset                   esito
006500990921     C                   reset                   wrkesito
006600010601     C*
006700010601     C                   exsr      opeini
006800060803     C                   exsr      rwfile
006900060906     c
007000110701     c                   clear                   fnlvSTds
007100110701     c                   movel     'C'           ilvSTtla
007200110701     c                   call      'FNLVSTR'
007300110706     c                   parm                    kpjba
007400110706     c                   parm                    fnlvSTds
007500010601     C*
007600060905     C                   seton                                        LR
007700010601     C***
007800010330     C*--------------------------------------------------------
007900060803     C* RWFILE  LEGGE TIVIN00R E SCRIVE FIARG                 *
008000010330     C*--------------------------------------------------------
008100060803     C     RWFILE        BEGSR
008200010330     C*
008300010330     C                   if        not %open(tivin00r)
008400010330     C                   open      tivin00r
008500010330     C                   endif
008600010330     C*
008700010604     C                   clear                   §CTROK
008800010604     C                   clear                   §CTRMO
008900010604     C                   clear                   §CTRNO
009000060720     C*
009100060720     C                   exsr      defcam
009200010330     C*
009300080109    1C                   DO        *HIVAL
009400010330     C*
009500010330     C                   READ      tivin00r                               70
009600010618     C*
009700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009800080109    2C                   if        vinflg = '1'
009900010618     C                   eval      flgOk = '1'
010000080109    2C                   endif
010100010618     C*
010200080109    2C                   if        vindta > *blanks
010300010330     C                   add       1             rrnum
010400010330     C*
010500080109    3C                   if        *in70 = *off and
010600010330     C                             (vinflg = *blanks
010700010330     C                              or vinflg = '0'
010800010330     C                              or vinflg = '2')
010900010330     C*
011000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011100080109    4C                   if        vinflg = *blanks or vinflg = '0'
011200010711     C                   clear                   vinmsg
011300080109    4C                   endif
011400010601     C*
011500060803     C* Esegue l'effettiva acquisizione dei dati
011600060803     C                   exsr      impfile
011700060803     C*
011800010601     C*
011900010604     C* Ebbene...
012000060905     C* *IN31 errore bloccante in scrittura/aggiornamento dati bartolini
012100060905     C* *IN33 errore bloccante in scrittura/aggiornamento dati Tellus
012200060803     C* *IN32 warning su anomalia sistemata o nn imiportante
012300010604     C*
012400060905     c* Dati per file tivin00r
012500080109    4c                   if        not *in31 and not *in33
012600060905     C                   ADD       1             §CTROK            7 0
012700080109    4c                   endif
012800010604     C   32              ADD       1             §CTRMO            7 0
012900060905     c   33
013000060905     Cor 31              ADD       1             §CTRNO            7 0
013100060905     c* Dati per file filte00f
013200080109    4c                   select
013300060905     c                   when      *in31
013400060906     c                   add       1             contaNSPEB
013500060905     c                   when      *in33
013600060905     c                   add       1             contaNSPET
013700060905     c                   when      *in32
013800060905     c                   add       1             contaNSPEw
013900080109    4c                   endsl
014000060905     c                   add       1             contaNSPr
014100020205     C*
014200060803     C* Scarico il buffer del FIARG
014300080109    4C                   IF        *IN31 = *off  and *in33 = *off
014400080109     C     Karg          CHAIN(e)  FIARG01L
014500080109   4ac                   if        not %error
014600080109     c
014700080109    5C                   IF        not %found(FIARG01L)  or
014800070124     c                             u_argtgi<>'M'
014900070124     c* Imposto i dati mancanti
015000080109    6c                   if        argcgi<>*blanks
015100070124     c                   add       1             contaNSPG
015200070124     C                   MOVEL     'A'           argTGI
015300070124     c                   movel     '1'           argttl
015400070124     c                   else
015500070124     C                   MOVEL     ' '           argTGI
015600070124     c                   movel     '2'           argttl
015700080109    6c                   endif
015800070124     c
015900060807     C                   EVAL      fiargds_u = fiargds
016000080109    6C                   IF        %found(FIARG01L)
016100060807     C                   UPDATE    FIARG000
016200060807     C                   ELSE
016300060807     C                   WRITE     FIARG000
016400080109    6C                   ENDIF
016500110701     c
016600110701     c* Chiamo pgm di controllo centro storico
016700110701     c                   if        olvstprop<>'N'
016800110701     c                   clear                   fnlvstds
016900110701     c                   eval      ilvstaas=arbaas
017000110701     c                   eval      ilvstlnp=arblnp
017100110701     c                   eval      ilvstnrs=arbnrs
017200110701     c                   eval      ilvstnsp=arbnsp
017300110701     c                   eval      ilvstmgs=arbmgs
017400110701     c                   eval      ilvstfin=arbfin
017500110701     c                   eval      ilvstcgi=argcgi
017600110701     c                   eval      ilvstpoc=argpoc
017700110701     c                   eval      ilvstdat=argdtvtel
017800110701     c                   call      'FNLVSTR'
017900110701     c                   parm                    kpjba
018000110701     c                   parm                    fnlvstds
018100110701     c                   endif
018200110701     c
018300080109    5C                   ENDIF
018400080109   4aC                   ENDIF
018500010604     C*
018600080109    4C                   endif
018700080109    3C                   endif
018800020722     C*
018900080109    3C                   if        *in31 = *off and *in33 =*off   and
019000010604     C                             *in32 = *off
019100010604     C                   eval      vinflg = '1'
019200010604     C                   else
019300010604     C                   eval      vinflg = '2'
019400080109    3C                   endif
019500010604     C*
019600010330     C                   else
019700010330     C                   eval      vinflg = '1'
019800080109    2C                   endif
019900010601     C*
020000010601     C  N70              update    tivin000
020100010330     C*
020200080109    1C  N70              ENDdo
020300010601     C
020400010601     C*
020500010330     C* Se non ci sono record con errori ...
020600010601     C                   if        §ctrno = 0 and
020700060803     C                             §ctrmo = 0
020800010330     C* ... restituisco esito OK.
020900010330     C                   eval      wrkesito = '0'
021000010330     C                   else
021100010330     C                   if        §ctrok > 0
021200010330     C                   eval      wrkesito = '1'
021300010330     C                   else
021400010615     C                   if        flgOk = '0'
021500010615     C                   eval      wrkesito = '2'
021600010615     C                   else
021700010615     C                   eval      wrkesito = '6'
021800010615     C                   endif
021900010330     C                   endif
022000010330     C                   endif
022100010330     C*
022200010330     C                   if        %open(tivin00r)
022300010330     C                   close     tivin00r
022400010330     C                   endif
022500010330     C*
022600060905     c* Aggiornamento file filte
022700060906     c                   EXSR      AGGFILTE
022800111018     c
022900111018     c* Aggiorno giro bolle di Perfect Order
023000111018     c                   clear                   fidg51ds
023100111018     c                   eval      d51fgsi=ltepoe
023200111018     c                   movel(P)  fidg51ds      kpjbu
023300111018     c                   call      'FIDG51R'
023400111018     c                   parm                    kpjba
023500111018     c
023600010330     C                   ENDSR
023700010330     C***
023800010601
023900010330     C*----------------------------------------------------*
024000020722     C*  IMPOSTAZIONE CAMPI COSTANTI
024100010330     C*----------------------------------------------------*
024200010330     C     DEFCAM        BEGSR
024300010330     C*
024400020204     C* Inizializzo il buffer del record da scrivere
024500060807     C                   CLEAR                   FIARGDS
024600020204     C* Imposto i valori di default...
024700020204     C* ... e poi verifico se sono stati passati come parametri
024800020204     C                   IF        vlrppt > *blanks
024900060803     C                   IF        %trim(%subst(vlrppt:1:1)) = 'T'              * Esecuzione in test
025000060804     C                   EVAL      flgTest = 'S'
025100060803     C                   ENDIF
025200020204     C                   ENDIF
025300020204     C*
025400010330     C                   ENDSR
025500010607     C*----------------------------------------------------*
025600060803     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIARG)
025700010607     C*----------------------------------------------------*
025800070112     C     IMPFILE       BEGSR
025900010607     C*
026000060905     C                   SETOFF                                       313233
026100060720     C*
026200060804     C* Ridefinisco i dati ricevuti da TELLUS
026300060804     C                   EVAL      dtllsi = vindta
026400060804     C*
026500060804     C* Valorizzazione buffer file FIARG
026600060905     c                   clear                   argATB
026700070201     c
026800070201     c* Verifico subito IDB per memorizzare la key di chain su FILTE
026900070201     c*  da aggiornare anche se tutti i record sono in errore blocante
027000070201    1c                   if        §tllsiIDB<>savtllIDB
027100070201     c                   clear                   erridb
027200070201     c                   clear                   pistr
027300070201     C                   EVAL      PiStr=§tllsiIDB
027400070201     C                   EXSR      CHKNUM
027500070201    2C                   IF        PiNum=*on
027600070201     C                   Z-ADD     PiVal         wIDB
027700070201     c
027800070201   x2C                   ELSE
027900070201     c* Errore non bloccante dati Bartolini
028000070201     c                   eval      erridb='E'
028100070201    2C                   ENDIF
028200070201     c
028300070201     c* Se non trovo l'ID elaborazione bartolini, errore non vincolante
028400070201    2c                   select
028500070201    2c                   when      widb>0 and keyidb>0 and widb<>keyidb
028600070201     c                   eval      erridb='D'
028700070201     c
028800070201     c* Errore non bloccante: idb diverso dal primo memorizzato
028900070201    2c                   when      widb>0 and keyidb=0
029000070201     c     widb          chain(N)  filte01l
029100070201    3c                   if        not %found(filte01l)
029200070201     c                   eval      erridb='N'
029300070201   x3c                   else
029400070201     c* salvo il campo key buono: non lo cambio +
029500070201     c                   eval      keyidb=widb
029600070201    3c                   endif
029700070201    2c                   endsl
029800070201     c
029900070201     c                   eval      savtllidb=§tllsiIDB
030000070201    1c                   endif
030100070124     c
030200070124     c* Se errore vincolante TELLUS, non carico il record
030300070424     c                   if        §tllsierr='02' or
030400070424     c                             §tllsierr='03' or
030500070424     c                             §tllsierr='04' or
030600070724     c                             §tllsierr='51' or
030700120313     c                             §tllsierr='52' or
030800120313     c                             §tllsierr='53'
030900070124     c                   goto      endimpo
031000070125     c                   endif
031100060905     c
031200060905     c* Ogni campo numerico in FIARG deve essere controllato prima di
031300060905     c*  essere mosso per evitare errori di dati decimali
031400060905     c
031500060905     c* Se elaborazione di TEST imposto nell'anno 9999
031600060804     C                   IF        flgTest = 'S'
031700060804     C                   MOVEL     *all'9'       argAAS
031800060804     C                   ELSE
031900060906     c                   clear                   pistr
032000060905     C                   EVAL      PiStr=§tllsiAAS
032100060905     C                   EXSR      CHKNUM
032200060905     C                   IF        PiNum=*on
032300060905     C                   Z-ADD     PiVal         argaas
032400060905     C                   ELSE
032500060905     c* Errore vincolante  dati Bartolini
032600060905     c                   seton                                        31
032700060905     C                   EVAL      vinmsg = %trimr(vinmsg)
032800060905     C                             + ' ' + 'ARGAAS'
032900060905     c                   goto      endimpo
033000060905     C                   ENDIF
033100060804     C                   ENDIF
033200060804     C*
033300060906     c                   clear                   pistr
033400060905     C                   EVAL      PiStr=§tllsiLNP
033500060905     C                   EXSR      CHKNUM
033600060905     C                   IF        PiNum=*on
033700060905     C                   Z-ADD     PiVal         argLNP
033800060905     C                   ELSE
033900060905     c* Errore vincolante  dati Bartolini
034000060905     c                   seton                                        31
034100060905     C                   EVAL      vinmsg = %trimr(vinmsg)
034200060905     C                             + ' ' + 'ARGLNP'
034300060905     c                   goto      endimpo
034400060905     C                   ENDIF
034500060905     C*
034600060906     c                   clear                   pistr
034700060905     C                   EVAL      PiStr=§tllsiNRS
034800060905     C                   EXSR      CHKNUM
034900060905     C                   IF        PiNum=*on
035000060905     C                   Z-ADD     PiVal         argNRS
035100060905     C                   ELSE
035200060905     c* Errore vincolante  dati Bartolini
035300060905     c                   seton                                        31
035400060905     C                   EVAL      vinmsg = %trimr(vinmsg)
035500060905     C                             + ' ' + 'ARGNRS'
035600060905     c                   goto      endimpo
035700060905     C                   ENDIF
035800060905     c
035900060906     c                   clear                   pistr
036000060905     C                   EVAL      PiStr=§tllsiNSP
036100060905     C                   EXSR      CHKNUM
036200060906    1C                   IF        PiNum=*on
036300060905     C                   Z-ADD     PiVal         argNSP
036400060906   x1C                   ELSE
036500060905     c* Errore vincolante  dati Bartolini
036600060905     c                   seton                                        31
036700060905     C                   EVAL      vinmsg = %trimr(vinmsg)
036800060905     C                             + ' ' + 'ARGNSP'
036900060905     c                   goto      endimpo
037000060906    1C                   ENDIF
037100060906     c* Verifico se esiste la bolla no per test
037200060906    0c                   if        argaas<>9999
037300060906     c     karg          chain     fnarb01l
037400060906    1c                   if        not %found(fnarb01l)
037500060905     c* Errore vincolante  dati Bartolini
037600060905     c                   seton                                        31
037700060905     C                   EVAL      vinmsg = %trimr(vinmsg)
037800060905     C                             + ' ' + 'NUMSPE'
037900060905     c                   goto      endimpo
038000060906   x1c                   else
038100060906 xx  C                   z-add     arblna        argLNA
038200060906     c
038300060906     c* Determino il POC (per ora) per vedere se uguale a quello impostato
038400080116    2c***                if        savlna<>arblna
038500080116     c***                clear                   fnlv55ds
038600080116     c***                movel     arblna        d55lin
038700080116     c***                movel     datcor        d55drf
038800080116     c***                movel     '6'           d55tpt
038900080116     c***                call      'FNLV55R'
039000080116     c***                parm                    fnlv55ds
039100080116     c***                movel     arblna        savlna
039200080116    2C***                endif
039300060906    1c                   endif
039400060906    0c                   endif
039500060905     c
039600060906     c                   clear                   pistr
039700060905     C                   EVAL      PiStr=§tllsiPOC
039800060905     C                   EXSR      CHKNUM
039900060905     C                   IF        PiNum=*on
040000080116     C                   Z-ADD     PiVal         argPOC
040100080116     c
040200060906     c* se diverso dal calcolato, errore non bloccante
040300080116     c***                if        argaas<>9999 and argpoc<>d55tfa
040400080116     C***                EVAL      vinmsg = %trimr(vinmsg)
040500080116     C***                          + ' ' + 'ARGPOC<>'
040600080116     c***                seton                                        32
040700080116     c***                z-add     d55tfa        argpoc
040800080116     c***                endif
040900060906     c
041000060905     C                   ELSE
041100060906     c* Errore non vincolante
041200080116     c***                seton                                        32
041300080116     C***                EVAL      vinmsg = %trimr(vinmsg)
041400080116     C***                          + ' ' + 'ARGPOC'
041500080116     c***                z-add     d55tfa        argpoc
041600080116     c* Es: ora errore vincolante visto che non ricalcolo 16 01 08
041700080116     c                   seton                                        31
041800080116     C                   EVAL      vinmsg = %trimr(vinmsg)
041900080116     C                             + ' ' + 'ARGPOC'
042000080116     c                   goto      endimpo
042100080116     C                   ENDIF
042200060905     c
042300060907     c* Registro l'errore su tivin
042400060907     c                   if        erridb<>' '
042500060907     c                   seton                                        32
042600060907     c                   select
042700060907     c* camp non numerico
042800060907     c                   when      erridb='E'
042900060907     C                   EVAL      vinmsg = %trimr(vinmsg)
043000060907     C                             + ' ' + 'ARGIDB'
043100060907     c
043200060907     c                   when      erridb='D'
043300060907     c* Widb non lo clearo lo memorizzo lo stesso nel file fiarg
043400060907     C                   EVAL      vinmsg = %trimr(vinmsg)
043500060907     C                             + ' ' + 'ARGIDB<>'
043600060907     c
043700060907     c                   when      erridb='N'
043800060907     c                   clear                   widb
043900070919     C                   EVAL      vinmsg = %trimr(vinmsg)
044000060907     C                             + ' ' + 'NoFILTE'
044100060907     c                   endsl
044200060907    2C                   ENDIF
044300060907     C                   Z-ADD     wIDB          argIDB
044400060905     c
044500060905     c
044600060905     c* Se c'e' codice giro conto
044700060905     C                   MOVEL     §TLLSICGI     argCGI
044800060906     c                   clear                   argSQG
044900070124     c                   clear                   argtgi
045000070124     c
045100070124     c***                if        argcgi<>*blanks
045200070124     c***                add       1             contaNSPG
045300070124     C***                MOVEL     'A'           argTGI
045400060905     c* Verifico se impostata la sequenza
045500070112     c*
045600070112     c*                  clear                   pistr
045700070112     C*                  EVAL      PiStr=§tllsiSQG
045800070112     C*                  EXSR      CHKNUM
045900070112     C*                  IF        PiNum=*on
046000070112     C*                  Z-ADD     PiVal         argSQG
046100070112     C*                  ELSE
046200070112     c*                  seton                                        32
046300070112     C*                  EVAL      vinmsg = %trimr(vinmsg)
046400070112     C*                            + ' ' + 'ARGSQG'
046500070112     c*                  endif
046600070124     c***                endif
046700060905     c
046800060807     C                   MOVEL     §TLLSILNO     argLNO
046900060804     C                   MOVEL     §TLLSIERR     argERR
047000060905     c                   clear                   argnst
047100060804     C*
047200060804     C* XLO
047300060804     C                   EVAL      PiStr=%trim(%subst(vindta:37:13))
047400060804     C                   EXSR      CHKNUM
047500060804     C                   IF        PiNum=*on
047600060804     C                   Z-ADD     PiVal         argXLO
047700060804     C                   ELSE
047800060905     C                   SETON                                        33
047900060905     C                   SETOff                                       32
048000060907     c* tolgo una spedizione dal conteggio non venendo caricata
048100070124     c***                if        argcgi<>*blanks
048200070124     c***                sub       1             contaNSPG
048300070124     c***                endif
048400060804     C                   EVAL      vinmsg = %trimr(vinmsg)
048500060804     C                             + ' ' + 'ARGXLO'
048600060905     c                   goto      endimpo
048700060804     C                   ENDIF
048800060804     C*
048900060804     C* YLA
049000060804     C                   EVAL      PiStr=%trim(%subst(vindta:50:13))
049100060804     C                   EXSR      CHKNUM
049200060804     C                   IF        PiNum=*on
049300060804     C                   Z-ADD     PiVal         argYLA
049400060804     C                   ELSE
049500060905     C                   SETON                                        33
049600060905     C                   SETOff                                       32
049700060907     c* tolgo una spedizione dal conteggio non venendo caricata
049800070124     c***                if        argcgi<>*blanks
049900070124     c***                sub       1             contaNSPG
050000070124     c***                endif
050100060804     C                   EVAL      vinmsg = %trimr(vinmsg)
050200060804     C                             + ' ' + 'ARGYLA'
050300060905     c                   goto      endimpo
050400060804     C                   ENDIF
050500070125     c
050600070125     c* Score normalizzazione
050700070125     c                   movel     §tllsisco     argsco
050800070125     c* Ora  elaborazione tellus
050900070125     c                   time                    arghvtel
051000070125     c* Data elaborazione tellus
051100070125     c                   time                    w0140            14 0
051200070125     c     *eur          move      w0140         dataeur
051300070125     c     *iso          movel     dataeur       argdtvtel
051400020204     C*
051500060905     C     ENDimpo       ENDSR
051600010330
051700010330     C*----------------------------------------------------*
051800010330     C*  CONTROLLO NUMERICITA' CAMPI
051900010330     C*----------------------------------------------------*
052000010330     C     CHKNUM        BEGSR
052100010330     C*
052200010606     C                   IF        PiDecChr = *blanks
052300010606     C                   EVAL      PiDecChr = ','
052400010606     C                   ENDIF
052500010606     C*
052600010606     C                   CALL(e)   'ISNUMERIC'
052700010330     C                   PARM                    PiStr            30
052800010606     C                   PARM                    PiDecChr          1
052900010330     C                   PARM      *ZEROS        PiVal            30 9
053000010330     C                   PARM      '0'           PiInt             1
053100010330     C                   PARM      '0'           PiNum             1
053200010330     C                   IF        %error
053300010606     C                   EVAL      PiNum=*off
053400010330     C                   ENDIF
053500010330     C*
053600010330     C                   ENDSR
053700010330     C***
053800060906     C*----------------------------------------------------*
053900060906     C*  AGGIORNAMENTO FILE FILTE01L
054000060906     C*----------------------------------------------------*
054100060906     C     AGGFILTE      BEGSR
054200060907     c* Solo se trovato
054300060907     c                   if        keyidb>0
054400070919     c     keyidb        chain     filte01l
054500060906     c                   eval      ltenspg=contanspg
054600060906     c                   eval      ltenspr=contanspr
054700060906     c                   eval      ltenspeb=contanspeb
054800060906     c                   eval      ltenspet=contanspet
054900060906     c                   eval      ltenspew=contanspew
055000060906     c* Aggiorno campi di uoload
055100060906     c                   eval      lteHDL=vlrhdl
055200060906     c                   eval      ltedatu=vlrdat
055300060906     c                   eval      lteorau=vlrora
055400060906     c                   eval      lteHDL=vlrhdl
055500080412     c*
055600080412     c                   update    filte000
055700080404     c*
055800060907     c                   endif
055900060907     c
056000060906     C                   ENDSR
056100070604
056200010601
056300060906     C*----------------------------------------------------*
056400060803      /TITLE Operazioni iniziali
056500060906     C*----------------------------------------------------*
056600010601     C     opeini        BEGSR
056700010601     C*
056800010601     C* Inizializzo flag e contatori operativi
056900010601     C                   movel     '0'           flgGiro           1
057000010615     C                   movel     '0'           flgOk             1
057100010601     C*
057200010601     C                   ENDSR
057300010601     C***
057400010330
057500060906     C*----------------------------------------------------*
057600000613     C     *inzsr        BEGSR
057700060906     C*----------------------------------------------------*
057800990910     C*
057900990910     C     *entry        plist
058000990920     C                   parm                    tivlrds
058100990921     C                   parm      wrkesito      esito
058200000724     C                   parm                    prmlit
058300000710     C                   parm                    prmfir
058400110701     c
058500110701     c                   eval      knsif='FILTRA201'
058600010330     C*
058700010330     C* CALCOLA LA DATA CORRENTE
058800080116     C**                 time                    wn14             14 0
058900080116     C**                 move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
059000080116     C**                 z-add     wn8           g08dat
059100080116     C**                 z-add     *zeros        g08inv
059200080116     C**                 movel     '0'           g08err
059300080116     C**                 call      'XSRDA8'
059400080116     C**                 parm                    wlbda8
059500080116     C**                 z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
059600060807     C*
059700060807     C* Definizione chiavi
059800060807     C*
059900060807     C* Chiave su FIARG01L - Completa
060000060905     C     KARG          KLIST
060100060807     C                   KFLD                    argAAS
060200060807     C                   KFLD                    argLNP
060300060807     C                   KFLD                    argNRS
060400060807     C                   KFLD                    argNSP
060500000613     C*
060600000613     C                   ENDSR
