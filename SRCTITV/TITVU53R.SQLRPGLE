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
014000170223     c                   clear                   orgpocgi
014100080109    6c                   endif
014200161128
014300161128      * se giro DIR non lo riporto su FNORG
014400161128    6c                   IF        orgcgi = 'DIR'
014500161128     c                   clear                   orgtgi
014600161128     c                   clear                   orgcgi
014700161128     c                   clear                   orgpocgi
014800161128     c                   clear                   orgdtvtel
014900161128     c                   clear                   orghvtel
015000161128    6c                   ENDIF
015100170414
015200170414      * se al momento dell'aggiornamento su FNORG ho la filiale ritiro diversa tra
015300170414      * ORG e ORM vuol dire che mentre il programma girava per calcolare il giro
015400170414      * l'ORM è stato dirottato, quindi non memorizzo il giro su ORG
015500170414    6c                   IF        ORMpor <> ORGpor
015600170414     c                   clear                   orgtgi
015700170414     c                   clear                   orgcgi
015800170414     c                   clear                   orgpocgi
015900170414     c                   clear                   orgdtvtel
016000170414     c                   clear                   orghvtel
016100170414    6c                   ENDIF
016200070124     c
016300160512    6C                   IF        %found(FNORG01L)
016400160512     c                   eval      u_orgcgi = orgcgi
016500160512     c                   eval      u_orgpocgi = orgpocgi
016600160512     c                   eval      u_orgtgi = orgtgi
016700160512     c                   eval      u_orgdtvtel = orgdtvtel
016800160512     c                   eval      u_orghvtel = orghvtel
016900160512     C                   UPDATE    FNORG000
017000060807     C                   ELSE
017100160512     C                   EVAL      fnorgds_u = fnorgds
017200160512     C                   WRITE     FNORG000
017300080109    6C                   ENDIF
017400110701     c
017500110701     c
017600080109    5C                   ENDIF
017700080109   4aC                   ENDIF
017800010604     C*
017900080109    4C                   endif
018000080109    3C                   endif
018100020722     C*
018200080109    3C                   if        *in31 = *off and *in33 =*off   and
018300010604     C                             *in32 = *off
018400010604     C                   eval      vinflg = '1'
018500010604     C                   else
018600010604     C                   eval      vinflg = '2'
018700080109    3C                   endif
018800010604     C*
018900010330     C                   else
019000010330     C                   eval      vinflg = '1'
019100080109    2C                   endif
019200010601     C*
019300010601     C  N70              update    tivin000
019400010330     C*
019500080109    1C  N70              ENDdo
019600010601     C
019700010601     C*
019800010330     C* Se non ci sono record con errori ...
019900010601     C                   if        §ctrno = 0 and
020000060803     C                             §ctrmo = 0
020100010330     C* ... restituisco esito OK.
020200010330     C                   eval      wrkesito = '0'
020300010330     C                   else
020400010330     C                   if        §ctrok > 0
020500010330     C                   eval      wrkesito = '1'
020600010330     C                   else
020700010615     C                   if        flgOk = '0'
020800010615     C                   eval      wrkesito = '2'
020900010615     C                   else
021000010615     C                   eval      wrkesito = '6'
021100010615     C                   endif
021200010330     C                   endif
021300010330     C                   endif
021400010330     C*
021500010330     C                   if        %open(tivin00r)
021600010330     C                   close     tivin00r
021700010330     C                   endif
021800010330     C*
021900160512     c* Aggiornamento file filto
022000160512     c                   EXSR      AGGFILTO
022100111018     c
022200111018     c
022300010330     C                   ENDSR
022400010330     C***
022500010601
022600010330     C*----------------------------------------------------*
022700020722     C*  IMPOSTAZIONE CAMPI COSTANTI
022800010330     C*----------------------------------------------------*
022900010330     C     DEFCAM        BEGSR
023000010330     C*
023100020204     C* Inizializzo il buffer del record da scrivere
023200160512     C                   CLEAR                   FNORGDS
023300020204     C* Imposto i valori di default...
023400020204     C* ... e poi verifico se sono stati passati come parametri
023500020204     C                   IF        vlrppt > *blanks
023600060803     C                   IF        %trim(%subst(vlrppt:1:1)) = 'T'              * Esecuzione in test
023700060804     C                   EVAL      flgTest = 'S'
023800060803     C                   ENDIF
023900020204     C                   ENDIF
024000020204     C*
024100010330     C                   ENDSR
024200010607     C*----------------------------------------------------*
024300160512     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNORG)
024400010607     C*----------------------------------------------------*
024500070112     C     IMPFILE       BEGSR
024600010607     C*
024700060905     C                   SETOFF                                       313233
024800060720     C*
024900060804     C* Ridefinisco i dati ricevuti da TELLUS
025000160512     C                   EVAL      dtlloi = vindta
025100060804     C*
025200160512     C* Valorizzazione buffer file FNORG
025300070201     c
025400070201     c* Verifico subito IDB per memorizzare la key di chain su FILTE
025500070201     c*  da aggiornare anche se tutti i record sono in errore blocante
025600160512    1c                   if        §tlloiIDB<>savtllIDB
025700070201     c                   clear                   erridb
025800070201     c                   clear                   pistr
025900160512     C                   EVAL      PiStr=§tlloiIDB
026000070201     C                   EXSR      CHKNUM
026100070201    2C                   IF        PiNum=*on
026200070201     C                   Z-ADD     PiVal         wIDB
026300070201     c
026400070201   x2C                   ELSE
026500070201     c* Errore non bloccante dati Bartolini
026600070201     c                   eval      erridb='E'
026700070201    2C                   ENDIF
026800070201     c
026900070201     c* Se non trovo l'ID elaborazione bartolini, errore non vincolante
027000070201    2c                   select
027100070201    2c                   when      widb>0 and keyidb>0 and widb<>keyidb
027200070201     c                   eval      erridb='D'
027300070201     c
027400070201     c* Errore non bloccante: idb diverso dal primo memorizzato
027500070201    2c                   when      widb>0 and keyidb=0
027600160512     c     widb          chain(N)  filto01l
027700160512    3c                   if        not %found(filto01l)
027800070201     c                   eval      erridb='N'
027900070201   x3c                   else
028000070201     c* salvo il campo key buono: non lo cambio +
028100070201     c                   eval      keyidb=widb
028200070201    3c                   endif
028300070201    2c                   endsl
028400070201     c
028500160512     c                   eval      savtllidb=§tlloiIDB
028600070201    1c                   endif
028700070124     c
028800070124     c* Se errore vincolante TELLUS, non carico il record
028900160512     c                   if        §tlloierr='02' or
029000160512     c                             §tlloierr='03' or
029100160512     c                             §tlloierr='04' or
029200160512     c                             §tlloierr='51' or
029300160512     c                             §tlloierr='52' or
029400160512     c                             §tlloierr='53'
029500070124     c                   goto      endimpo
029600070125     c                   endif
029700060905     c
029800160512     c* Ogni campo numerico in FNORG deve essere controllato prima di
029900060905     c*  essere mosso per evitare errori di dati decimali
030000060905     c
030100160512     c* Se elaborazione di TEST imposto 99 nella serie
030200060804     C                   IF        flgTest = 'S'
030300160512     C                   MOVEL     *all'9'       orgNSR
030400060804     C                   ELSE
030500060906     c                   clear                   pistr
030600160512     C                   EVAL      PiStr=§tlloiNSR
030700060905     C                   EXSR      CHKNUM
030800060905     C                   IF        PiNum=*on
030900160512     C                   Z-ADD     PiVal         orgnsr
031000060905     C                   ELSE
031100060905     c* Errore vincolante  dati Bartolini
031200060905     c                   seton                                        31
031300060905     C                   EVAL      vinmsg = %trimr(vinmsg)
031400160512     C                             + ' ' + 'ORGNSR'
031500060905     c                   goto      endimpo
031600060905     C                   ENDIF
031700060804     C                   ENDIF
031800060804     C*
031900060906     c                   clear                   pistr
032000160512     C                   EVAL      PiStr=§tlloiPOE
032100060905     C                   EXSR      CHKNUM
032200060905     C                   IF        PiNum=*on
032300160512     C                   Z-ADD     PiVal         orgPOE
032400060905     C                   ELSE
032500060905     c* Errore vincolante  dati Bartolini
032600060905     c                   seton                                        31
032700060905     C                   EVAL      vinmsg = %trimr(vinmsg)
032800160512     C                             + ' ' + 'ORGPOE'
032900060905     c                   goto      endimpo
033000060905     C                   ENDIF
033100060905     C*
033200060906     c                   clear                   pistr
033300160512     C                   EVAL      PiStr=§tlloiAAS
033400060905     C                   EXSR      CHKNUM
033500060905     C                   IF        PiNum=*on
033600160512     C                   Z-ADD     PiVal         waas              2 0
033700060905     C                   ELSE
033800160512     c* Errore NON vincolante  dati Bartolini (non ce l'ho su FNORG)
033900160512     c                   seton                                        32
034000060905     C                   EVAL      vinmsg = %trimr(vinmsg)
034100160512     C                             + ' ' + 'AAS'
034200060905     c                   goto      endimpo
034300060905     C                   ENDIF
034400060905     c
034500060906     c                   clear                   pistr
034600160512     C                   EVAL      PiStr=§tlloiNOR
034700060905     C                   EXSR      CHKNUM
034800060906    1C                   IF        PiNum=*on
034900160512     C                   Z-ADD     PiVal         orgNOR
035000060906   x1C                   ELSE
035100060905     c* Errore vincolante  dati Bartolini
035200060905     c                   seton                                        31
035300060905     C                   EVAL      vinmsg = %trimr(vinmsg)
035400160512     C                             + ' ' + 'ORGNOR'
035500060905     c                   goto      endimpo
035600060906    1C                   ENDIF
035700160512     c
035800160512     c                   clear                   pistr
035900160512     C                   EVAL      PiStr=§tlloiNRV
036000160512     C                   EXSR      CHKNUM
036100160512    1C                   IF        PiNum=*on
036200160512     C                   Z-ADD     PiVal         orgNRV
036300160512   x1C                   ELSE
036400160512     c* Errore vincolante  dati Bartolini
036500160512     c                   seton                                        31
036600160512     C                   EVAL      vinmsg = %trimr(vinmsg)
036700160512     C                             + ' ' + 'ORGNRV'
036800160512     c                   goto      endimpo
036900160512    1C                   ENDIF
037000160512     c* Verifico se esiste l'ORM    no per test
037100160512    0c                   if        orgnsr<>99
037200160512     c     korg          chain     fnorm01l
037300160512    1c                   if        not %found(fnorm01l)
037400060905     c* Errore vincolante  dati Bartolini
037500060905     c                   seton                                        31
037600060905     C                   EVAL      vinmsg = %trimr(vinmsg)
037700160512     C                             + ' ' + 'NUMORM'
037800060905     c                   goto      endimpo
037900060906   x1c                   else
038000160512 xx  C                   z-add     ormpor        orgPOR
038100060906     c
038200060906    1c                   endif
038300060906    0c                   endif
038400060905     c
038500060906     c                   clear                   pistr
038600160512     C                   EVAL      PiStr=§tlloiPOR
038700060905     C                   EXSR      CHKNUM
038800060905     C                   IF        PiNum=*on
038900160512     C                   Z-ADD     PiVal         orgPOR
039000080116     c
039100060906     c
039200060905     C                   ELSE
039300160512     c*  errore vincolante
039400080116     c                   seton                                        31
039500080116     C                   EVAL      vinmsg = %trimr(vinmsg)
039600160512     C                             + ' ' + 'ORGPOR'
039700080116     c                   goto      endimpo
039800080116     C                   ENDIF
039900060905     c
040000060907     c* Registro l'errore su tivin
040100060907     c                   if        erridb<>' '
040200060907     c                   seton                                        32
040300060907     c                   select
040400060907     c* camp non numerico
040500060907     c                   when      erridb='E'
040600060907     C                   EVAL      vinmsg = %trimr(vinmsg)
040700160512     C                             + ' ' + 'WIDB'
040800060907     c
040900060907     c                   when      erridb='D'
041000160512     c
041100060907     C                   EVAL      vinmsg = %trimr(vinmsg)
041200160512     C                             + ' ' + 'WIDB<>'
041300060907     c
041400060907     c                   when      erridb='N'
041500060907     c                   clear                   widb
041600070919     C                   EVAL      vinmsg = %trimr(vinmsg)
041700160512     C                             + ' ' + 'NoFILTO'
041800060907     c                   endsl
041900060907    2C                   ENDIF
042000060905     c
042100060905     c
042200060905     c* Se c'e' codice giro conto
042300160512     C                   MOVEL     §TLLOICGI     orgCGI
042400160512     c                   clear                   orgtgi
042500060905     c
042600070125     c
042700070125     c* Ora  elaborazione tellus
042800160512     c                   time                    orghvtel
042900070125     c* Data elaborazione tellus
043000070125     c                   time                    w0140            14 0
043100070125     c     *eur          move      w0140         dataeur
043200160512     c     *iso          movel     dataeur       orgdtvtel
043300020204     C*
043400060905     C     ENDimpo       ENDSR
043500010330
043600010330     C*----------------------------------------------------*
043700010330     C*  CONTROLLO NUMERICITA' CAMPI
043800010330     C*----------------------------------------------------*
043900010330     C     CHKNUM        BEGSR
044000010330     C*
044100010606     C                   IF        PiDecChr = *blanks
044200010606     C                   EVAL      PiDecChr = ','
044300010606     C                   ENDIF
044400010606     C*
044500010606     C                   CALL(e)   'ISNUMERIC'
044600010330     C                   PARM                    PiStr            30
044700010606     C                   PARM                    PiDecChr          1
044800010330     C                   PARM      *ZEROS        PiVal            30 9
044900010330     C                   PARM      '0'           PiInt             1
045000010330     C                   PARM      '0'           PiNum             1
045100010330     C                   IF        %error
045200010606     C                   EVAL      PiNum=*off
045300010330     C                   ENDIF
045400010330     C*
045500010330     C                   ENDSR
045600010330     C***
045700060906     C*----------------------------------------------------*
045800060906     C*  AGGIORNAMENTO FILE FILTE01L
045900060906     C*----------------------------------------------------*
046000160512     C     AGGFILTO      BEGSR
046100060907     c* Solo se trovato
046200060907     c                   if        keyidb>0
046300160512     c     keyidb        chain     filto01l
046400160512     c                   eval      ltonrig=contanrig
046500160512     c                   eval      ltonrir=contanrir
046600160512     c                   eval      ltonrieb=contanrieb
046700160512     c                   eval      ltonriet=contanriet
046800160512     c                   eval      ltonriew=contanriew
046900060906     c* Aggiorno campi di uoload
047000160512     c                   eval      ltoHDL=vlrhdl
047100160512     c                   eval      ltodatu=vlrdat
047200160512     c                   eval      ltoorau=vlrora
047300160512     c                   eval      ltoHDL=vlrhdl
047400080412     c*
047500160512     c                   update    filto000
047600080404     c*
047700060907     c                   endif
047800060907     c
047900060906     C                   ENDSR
048000070604
048100010601
048200060906     C*----------------------------------------------------*
048300060803      /TITLE Operazioni iniziali
048400060906     C*----------------------------------------------------*
048500010601     C     opeini        BEGSR
048600010601     C*
048700010601     C* Inizializzo flag e contatori operativi
048800010601     C                   movel     '0'           flgGiro           1
048900010615     C                   movel     '0'           flgOk             1
049000010601     C*
049100010601     C                   ENDSR
049200010601     C***
049300010330
049400060906     C*----------------------------------------------------*
049500000613     C     *inzsr        BEGSR
049600060906     C*----------------------------------------------------*
049700990910     C*
049800990910     C     *entry        plist
049900990920     C                   parm                    tivlrds
050000990921     C                   parm      wrkesito      esito
050100000724     C                   parm                    prmlit
050200000710     C                   parm                    prmfir
050300110701     c
050400060807     C*
050500060807     C* Definizione chiavi
050600060807     C*
050700160512     C* Chiave su FNORG01L - Completa
050800160512     C     KORG          KLIST
050900160512     C                   KFLD                    orgPOE
051000160512     C                   KFLD                    orgNSR
051100160512     C                   KFLD                    orgNOR
051200160512     C                   KFLD                    orgNRV
051300000613     C*
051400000613     C                   ENDSR
