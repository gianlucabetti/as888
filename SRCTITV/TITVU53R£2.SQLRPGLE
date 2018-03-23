000100160512      /TITLE Upload via Internet: traduzione in FNORG00F
000200121218     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300000313     F*
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600160512     FFNORG01L  uF A E           K DISK    prefix(u_)
000700160512     FFnorm01l  iF   E           K DISK
000800160512     FFilto01l  uF   E           K DISK
000900010330     D*----------------------------------------------------
001000070124     D* DICHIARAZIONE VARIABILI DI WRK
001100010330     D*----------------------------------------------------
001200070124
001300010330     D psds           sds
001400010330     D  procname         *PROC
001500010330     D tivlrds       e ds                  extname(tivlr00f)
001600160512     D dtlloi        e ds
001700160512     D fnorgds       e ds                  extname(fnorg00f)
001800160512     D fnorgds_u     e ds                  extname(fnorg00f) prefix(u_)
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
003100160512     D wIDB            s                   like(ltoIDB)
003200160512     D keyidb          s                   like(ltoIDB)
003300160512     D contaNRIG       s                   like(ltoNRIG)
003400160512     D contaNRIEB      s                   like(ltoNRIG)
003500160512     D contaNRIET      s                   like(ltoNRIG)
003600160512     D contaNRIEW      s                   like(ltoNRIG)
003700160512     D contaNRIR       s                   like(ltoNRIG)
003800160512     D SAVtllidb       s                   like(§tlloiidb)
003900060906     d                                     inz('ccllffo')
004000060907     D ErrIDB          s              1    inz
004100110701     D kpjba         E DS                  inz
004200000313     D*
004300060906     D*------------------
004400080109     C*
004500080109     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
004600080109     C
004700080109     C/EXEC SQL
004800080109     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
004900080109     C/END-EXEC
005000990915     C                   time                    wrkdata
005100990915     C                   time                    wrkora
005200000913     C                   reset                   rrnum
005300990921     C                   reset                   esito
005400990921     C                   reset                   wrkesito
005500010601     C*
005600010601     C                   exsr      opeini
005700060803     C                   exsr      rwfile
005800060906     c
005900010601     C*
006000060905     C                   seton                                        LR
006100010601     C***
006200010330     C*--------------------------------------------------------
006300060803     C* RWFILE  LEGGE TIVIN00R E SCRIVE FIARG                 *
006400010330     C*--------------------------------------------------------
006500060803     C     RWFILE        BEGSR
006600010330     C*
006700010330     C                   if        not %open(tivin00r)
006800010330     C                   open      tivin00r
006900010330     C                   endif
007000010330     C*
007100010604     C                   clear                   §CTROK
007200010604     C                   clear                   §CTRMO
007300010604     C                   clear                   §CTRNO
007400060720     C*
007500060720     C                   exsr      defcam
007600010330     C*
007700080109    1C                   DO        *HIVAL
007800010330     C*
007900010330     C                   READ      tivin00r                               70
008000010618     C*
008100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
008200080109    2C                   if        vinflg = '1'
008300010618     C                   eval      flgOk = '1'
008400080109    2C                   endif
008500010618     C*
008600080109    2C                   if        vindta > *blanks
008700010330     C                   add       1             rrnum
008800010330     C*
008900080109    3C                   if        *in70 = *off and
009000010330     C                             (vinflg = *blanks
009100010330     C                              or vinflg = '0'
009200010330     C                              or vinflg = '2')
009300010330     C*
009400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
009500080109    4C                   if        vinflg = *blanks or vinflg = '0'
009600010711     C                   clear                   vinmsg
009700080109    4C                   endif
009800010601     C*
009900060803     C* Esegue l'effettiva acquisizione dei dati
010000060803     C                   exsr      impfile
010100060803     C*
010200010601     C*
010300010604     C* Ebbene...
010400060905     C* *IN31 errore bloccante in scrittura/aggiornamento dati bartolini
010500060905     C* *IN33 errore bloccante in scrittura/aggiornamento dati Tellus
010600060803     C* *IN32 warning su anomalia sistemata o nn imiportante
010700010604     C*
010800060905     c* Dati per file tivin00r
010900080109    4c                   if        not *in31 and not *in33
011000060905     C                   ADD       1             §CTROK            7 0
011100080109    4c                   endif
011200010604     C   32              ADD       1             §CTRMO            7 0
011300060905     c   33
011400060905     Cor 31              ADD       1             §CTRNO            7 0
011500160512     c* Dati per file filto00f
011600080109    4c                   select
011700060905     c                   when      *in31
011800160512     c                   add       1             contaNRIEB
011900060905     c                   when      *in33
012000160512     c                   add       1             contaNRIET
012100060905     c                   when      *in32
012200160512     c                   add       1             contaNRIEw
012300080109    4c                   endsl
012400160512     c                   add       1             contaNRIr
012500020205     C*
012600160512     C* Scarico il buffer del FNORG
012700080109    4C                   IF        *IN31 = *off  and *in33 = *off
012800160512     C     Korg          CHAIN(e)  FNORG01L
012900080109   4ac                   if        not %error
013000080109     c
013100160512    5C                   IF        not %found(FNORG01L)  or
013200160512     c                             u_orgtgi=' '
013300070124     c* Imposto i dati mancanti
013400160512    6c                   if        orgcgi<>*blanks
013500160512     c                   add       1             contaNRIG
013600160512     C                   MOVEL     'T'           orgTGI
013700160512     c                   eval      orgpocgi = orgpor
013800070124     c                   else
013900160512     C                   MOVEL     ' '           orgTGI
014000080109    6c                   endif
014100161128
014200161128      * se giro DIR non lo riporto su FNORG
014300161128    6c                   IF        orgcgi = 'DIR'
014400161128     c                   clear                   orgtgi
014500161128     c                   clear                   orgcgi
014600161128     c                   clear                   orgpocgi
014700161128     c                   clear                   orgdtvtel
014800161128     c                   clear                   orghvtel
014900161128    6c                   ENDIF
015000070124     c
015100160512    6C                   IF        %found(FNORG01L)
015200160512     c                   eval      u_orgcgi = orgcgi
015300160512     c                   eval      u_orgpocgi = orgpocgi
015400160512     c                   eval      u_orgtgi = orgtgi
015500160512     c                   eval      u_orgdtvtel = orgdtvtel
015600160512     c                   eval      u_orghvtel = orghvtel
015700160512     C                   UPDATE    FNORG000
015800060807     C                   ELSE
015900160512     C                   EVAL      fnorgds_u = fnorgds
016000160512     C                   WRITE     FNORG000
016100080109    6C                   ENDIF
016200110701     c
016300110701     c
016400080109    5C                   ENDIF
016500080109   4aC                   ENDIF
016600010604     C*
016700080109    4C                   endif
016800080109    3C                   endif
016900020722     C*
017000080109    3C                   if        *in31 = *off and *in33 =*off   and
017100010604     C                             *in32 = *off
017200010604     C                   eval      vinflg = '1'
017300010604     C                   else
017400010604     C                   eval      vinflg = '2'
017500080109    3C                   endif
017600010604     C*
017700010330     C                   else
017800010330     C                   eval      vinflg = '1'
017900080109    2C                   endif
018000010601     C*
018100010601     C  N70              update    tivin000
018200010330     C*
018300080109    1C  N70              ENDdo
018400010601     C
018500010601     C*
018600010330     C* Se non ci sono record con errori ...
018700010601     C                   if        §ctrno = 0 and
018800060803     C                             §ctrmo = 0
018900010330     C* ... restituisco esito OK.
019000010330     C                   eval      wrkesito = '0'
019100010330     C                   else
019200010330     C                   if        §ctrok > 0
019300010330     C                   eval      wrkesito = '1'
019400010330     C                   else
019500010615     C                   if        flgOk = '0'
019600010615     C                   eval      wrkesito = '2'
019700010615     C                   else
019800010615     C                   eval      wrkesito = '6'
019900010615     C                   endif
020000010330     C                   endif
020100010330     C                   endif
020200010330     C*
020300010330     C                   if        %open(tivin00r)
020400010330     C                   close     tivin00r
020500010330     C                   endif
020600010330     C*
020700160512     c* Aggiornamento file filto
020800160512     c                   EXSR      AGGFILTO
020900111018     c
021000111018     c
021100010330     C                   ENDSR
021200010330     C***
021300010601
021400010330     C*----------------------------------------------------*
021500020722     C*  IMPOSTAZIONE CAMPI COSTANTI
021600010330     C*----------------------------------------------------*
021700010330     C     DEFCAM        BEGSR
021800010330     C*
021900020204     C* Inizializzo il buffer del record da scrivere
022000160512     C                   CLEAR                   FNORGDS
022100020204     C* Imposto i valori di default...
022200020204     C* ... e poi verifico se sono stati passati come parametri
022300020204     C                   IF        vlrppt > *blanks
022400060803     C                   IF        %trim(%subst(vlrppt:1:1)) = 'T'              * Esecuzione in test
022500060804     C                   EVAL      flgTest = 'S'
022600060803     C                   ENDIF
022700020204     C                   ENDIF
022800020204     C*
022900010330     C                   ENDSR
023000010607     C*----------------------------------------------------*
023100160512     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNORG)
023200010607     C*----------------------------------------------------*
023300070112     C     IMPFILE       BEGSR
023400010607     C*
023500060905     C                   SETOFF                                       313233
023600060720     C*
023700060804     C* Ridefinisco i dati ricevuti da TELLUS
023800160512     C                   EVAL      dtlloi = vindta
023900060804     C*
024000160512     C* Valorizzazione buffer file FNORG
024100070201     c
024200070201     c* Verifico subito IDB per memorizzare la key di chain su FILTE
024300070201     c*  da aggiornare anche se tutti i record sono in errore blocante
024400160512    1c                   if        §tlloiIDB<>savtllIDB
024500070201     c                   clear                   erridb
024600070201     c                   clear                   pistr
024700160512     C                   EVAL      PiStr=§tlloiIDB
024800070201     C                   EXSR      CHKNUM
024900070201    2C                   IF        PiNum=*on
025000070201     C                   Z-ADD     PiVal         wIDB
025100070201     c
025200070201   x2C                   ELSE
025300070201     c* Errore non bloccante dati Bartolini
025400070201     c                   eval      erridb='E'
025500070201    2C                   ENDIF
025600070201     c
025700070201     c* Se non trovo l'ID elaborazione bartolini, errore non vincolante
025800070201    2c                   select
025900070201    2c                   when      widb>0 and keyidb>0 and widb<>keyidb
026000070201     c                   eval      erridb='D'
026100070201     c
026200070201     c* Errore non bloccante: idb diverso dal primo memorizzato
026300070201    2c                   when      widb>0 and keyidb=0
026400160512     c     widb          chain(N)  filto01l
026500160512    3c                   if        not %found(filto01l)
026600070201     c                   eval      erridb='N'
026700070201   x3c                   else
026800070201     c* salvo il campo key buono: non lo cambio +
026900070201     c                   eval      keyidb=widb
027000070201    3c                   endif
027100070201    2c                   endsl
027200070201     c
027300160512     c                   eval      savtllidb=§tlloiIDB
027400070201    1c                   endif
027500070124     c
027600070124     c* Se errore vincolante TELLUS, non carico il record
027700160512     c                   if        §tlloierr='02' or
027800160512     c                             §tlloierr='03' or
027900160512     c                             §tlloierr='04' or
028000160512     c                             §tlloierr='51' or
028100160512     c                             §tlloierr='52' or
028200160512     c                             §tlloierr='53'
028300070124     c                   goto      endimpo
028400070125     c                   endif
028500060905     c
028600160512     c* Ogni campo numerico in FNORG deve essere controllato prima di
028700060905     c*  essere mosso per evitare errori di dati decimali
028800060905     c
028900160512     c* Se elaborazione di TEST imposto 99 nella serie
029000060804     C                   IF        flgTest = 'S'
029100160512     C                   MOVEL     *all'9'       orgNSR
029200060804     C                   ELSE
029300060906     c                   clear                   pistr
029400160512     C                   EVAL      PiStr=§tlloiNSR
029500060905     C                   EXSR      CHKNUM
029600060905     C                   IF        PiNum=*on
029700160512     C                   Z-ADD     PiVal         orgnsr
029800060905     C                   ELSE
029900060905     c* Errore vincolante  dati Bartolini
030000060905     c                   seton                                        31
030100060905     C                   EVAL      vinmsg = %trimr(vinmsg)
030200160512     C                             + ' ' + 'ORGNSR'
030300060905     c                   goto      endimpo
030400060905     C                   ENDIF
030500060804     C                   ENDIF
030600060804     C*
030700060906     c                   clear                   pistr
030800160512     C                   EVAL      PiStr=§tlloiPOE
030900060905     C                   EXSR      CHKNUM
031000060905     C                   IF        PiNum=*on
031100160512     C                   Z-ADD     PiVal         orgPOE
031200060905     C                   ELSE
031300060905     c* Errore vincolante  dati Bartolini
031400060905     c                   seton                                        31
031500060905     C                   EVAL      vinmsg = %trimr(vinmsg)
031600160512     C                             + ' ' + 'ORGPOE'
031700060905     c                   goto      endimpo
031800060905     C                   ENDIF
031900060905     C*
032000060906     c                   clear                   pistr
032100160512     C                   EVAL      PiStr=§tlloiAAS
032200060905     C                   EXSR      CHKNUM
032300060905     C                   IF        PiNum=*on
032400160512     C                   Z-ADD     PiVal         waas              2 0
032500060905     C                   ELSE
032600160512     c* Errore NON vincolante  dati Bartolini (non ce l'ho su FNORG)
032700160512     c                   seton                                        32
032800060905     C                   EVAL      vinmsg = %trimr(vinmsg)
032900160512     C                             + ' ' + 'AAS'
033000060905     c                   goto      endimpo
033100060905     C                   ENDIF
033200060905     c
033300060906     c                   clear                   pistr
033400160512     C                   EVAL      PiStr=§tlloiNOR
033500060905     C                   EXSR      CHKNUM
033600060906    1C                   IF        PiNum=*on
033700160512     C                   Z-ADD     PiVal         orgNOR
033800060906   x1C                   ELSE
033900060905     c* Errore vincolante  dati Bartolini
034000060905     c                   seton                                        31
034100060905     C                   EVAL      vinmsg = %trimr(vinmsg)
034200160512     C                             + ' ' + 'ORGNOR'
034300060905     c                   goto      endimpo
034400060906    1C                   ENDIF
034500160512     c
034600160512     c                   clear                   pistr
034700160512     C                   EVAL      PiStr=§tlloiNRV
034800160512     C                   EXSR      CHKNUM
034900160512    1C                   IF        PiNum=*on
035000160512     C                   Z-ADD     PiVal         orgNRV
035100160512   x1C                   ELSE
035200160512     c* Errore vincolante  dati Bartolini
035300160512     c                   seton                                        31
035400160512     C                   EVAL      vinmsg = %trimr(vinmsg)
035500160512     C                             + ' ' + 'ORGNRV'
035600160512     c                   goto      endimpo
035700160512    1C                   ENDIF
035800160512     c* Verifico se esiste l'ORM    no per test
035900160512    0c                   if        orgnsr<>99
036000160512     c     korg          chain     fnorm01l
036100160512    1c                   if        not %found(fnorm01l)
036200060905     c* Errore vincolante  dati Bartolini
036300060905     c                   seton                                        31
036400060905     C                   EVAL      vinmsg = %trimr(vinmsg)
036500160512     C                             + ' ' + 'NUMORM'
036600060905     c                   goto      endimpo
036700060906   x1c                   else
036800160512 xx  C                   z-add     ormpor        orgPOR
036900060906     c
037000060906    1c                   endif
037100060906    0c                   endif
037200060905     c
037300060906     c                   clear                   pistr
037400160512     C                   EVAL      PiStr=§tlloiPOR
037500060905     C                   EXSR      CHKNUM
037600060905     C                   IF        PiNum=*on
037700160512     C                   Z-ADD     PiVal         orgPOR
037800080116     c
037900060906     c
038000060905     C                   ELSE
038100160512     c*  errore vincolante
038200080116     c                   seton                                        31
038300080116     C                   EVAL      vinmsg = %trimr(vinmsg)
038400160512     C                             + ' ' + 'ORGPOR'
038500080116     c                   goto      endimpo
038600080116     C                   ENDIF
038700060905     c
038800060907     c* Registro l'errore su tivin
038900060907     c                   if        erridb<>' '
039000060907     c                   seton                                        32
039100060907     c                   select
039200060907     c* camp non numerico
039300060907     c                   when      erridb='E'
039400060907     C                   EVAL      vinmsg = %trimr(vinmsg)
039500160512     C                             + ' ' + 'WIDB'
039600060907     c
039700060907     c                   when      erridb='D'
039800160512     c
039900060907     C                   EVAL      vinmsg = %trimr(vinmsg)
040000160512     C                             + ' ' + 'WIDB<>'
040100060907     c
040200060907     c                   when      erridb='N'
040300060907     c                   clear                   widb
040400070919     C                   EVAL      vinmsg = %trimr(vinmsg)
040500160512     C                             + ' ' + 'NoFILTO'
040600060907     c                   endsl
040700060907    2C                   ENDIF
040800060905     c
040900060905     c
041000060905     c* Se c'e' codice giro conto
041100160512     C                   MOVEL     §TLLOICGI     orgCGI
041200160512     c                   clear                   orgtgi
041300060905     c
041400070125     c
041500070125     c* Ora  elaborazione tellus
041600160512     c                   time                    orghvtel
041700070125     c* Data elaborazione tellus
041800070125     c                   time                    w0140            14 0
041900070125     c     *eur          move      w0140         dataeur
042000160512     c     *iso          movel     dataeur       orgdtvtel
042100020204     C*
042200060905     C     ENDimpo       ENDSR
042300010330
042400010330     C*----------------------------------------------------*
042500010330     C*  CONTROLLO NUMERICITA' CAMPI
042600010330     C*----------------------------------------------------*
042700010330     C     CHKNUM        BEGSR
042800010330     C*
042900010606     C                   IF        PiDecChr = *blanks
043000010606     C                   EVAL      PiDecChr = ','
043100010606     C                   ENDIF
043200010606     C*
043300010606     C                   CALL(e)   'ISNUMERIC'
043400010330     C                   PARM                    PiStr            30
043500010606     C                   PARM                    PiDecChr          1
043600010330     C                   PARM      *ZEROS        PiVal            30 9
043700010330     C                   PARM      '0'           PiInt             1
043800010330     C                   PARM      '0'           PiNum             1
043900010330     C                   IF        %error
044000010606     C                   EVAL      PiNum=*off
044100010330     C                   ENDIF
044200010330     C*
044300010330     C                   ENDSR
044400010330     C***
044500060906     C*----------------------------------------------------*
044600060906     C*  AGGIORNAMENTO FILE FILTE01L
044700060906     C*----------------------------------------------------*
044800160512     C     AGGFILTO      BEGSR
044900060907     c* Solo se trovato
045000060907     c                   if        keyidb>0
045100160512     c     keyidb        chain     filto01l
045200160512     c                   eval      ltonrig=contanrig
045300160512     c                   eval      ltonrir=contanrir
045400160512     c                   eval      ltonrieb=contanrieb
045500160512     c                   eval      ltonriet=contanriet
045600160512     c                   eval      ltonriew=contanriew
045700060906     c* Aggiorno campi di uoload
045800160512     c                   eval      ltoHDL=vlrhdl
045900160512     c                   eval      ltodatu=vlrdat
046000160512     c                   eval      ltoorau=vlrora
046100160512     c                   eval      ltoHDL=vlrhdl
046200080412     c*
046300160512     c                   update    filto000
046400080404     c*
046500060907     c                   endif
046600060907     c
046700060906     C                   ENDSR
046800070604
046900010601
047000060906     C*----------------------------------------------------*
047100060803      /TITLE Operazioni iniziali
047200060906     C*----------------------------------------------------*
047300010601     C     opeini        BEGSR
047400010601     C*
047500010601     C* Inizializzo flag e contatori operativi
047600010601     C                   movel     '0'           flgGiro           1
047700010615     C                   movel     '0'           flgOk             1
047800010601     C*
047900010601     C                   ENDSR
048000010601     C***
048100010330
048200060906     C*----------------------------------------------------*
048300000613     C     *inzsr        BEGSR
048400060906     C*----------------------------------------------------*
048500990910     C*
048600990910     C     *entry        plist
048700990920     C                   parm                    tivlrds
048800990921     C                   parm      wrkesito      esito
048900000724     C                   parm                    prmlit
049000000710     C                   parm                    prmfir
049100110701     c
049200060807     C*
049300060807     C* Definizione chiavi
049400060807     C*
049500160512     C* Chiave su FNORG01L - Completa
049600160512     C     KORG          KLIST
049700160512     C                   KFLD                    orgPOE
049800160512     C                   KFLD                    orgNSR
049900160512     C                   KFLD                    orgNOR
050000160512     C                   KFLD                    orgNRV
050100000613     C*
050200000613     C                   ENDSR
