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
014100070124     c
014200160512    6C                   IF        %found(FNORG01L)
014300160512     c                   eval      u_orgcgi = orgcgi
014400160512     c                   eval      u_orgpocgi = orgpocgi
014500160512     c                   eval      u_orgtgi = orgtgi
014600160512     c                   eval      u_orgdtvtel = orgdtvtel
014700160512     c                   eval      u_orghvtel = orghvtel
014800160512     C                   UPDATE    FNORG000
014900060807     C                   ELSE
015000160512     C                   EVAL      fnorgds_u = fnorgds
015100160512     C                   WRITE     FNORG000
015200080109    6C                   ENDIF
015300110701     c
015400110701     c
015500080109    5C                   ENDIF
015600080109   4aC                   ENDIF
015700010604     C*
015800080109    4C                   endif
015900080109    3C                   endif
016000020722     C*
016100080109    3C                   if        *in31 = *off and *in33 =*off   and
016200010604     C                             *in32 = *off
016300010604     C                   eval      vinflg = '1'
016400010604     C                   else
016500010604     C                   eval      vinflg = '2'
016600080109    3C                   endif
016700010604     C*
016800010330     C                   else
016900010330     C                   eval      vinflg = '1'
017000080109    2C                   endif
017100010601     C*
017200010601     C  N70              update    tivin000
017300010330     C*
017400080109    1C  N70              ENDdo
017500010601     C
017600010601     C*
017700010330     C* Se non ci sono record con errori ...
017800010601     C                   if        §ctrno = 0 and
017900060803     C                             §ctrmo = 0
018000010330     C* ... restituisco esito OK.
018100010330     C                   eval      wrkesito = '0'
018200010330     C                   else
018300010330     C                   if        §ctrok > 0
018400010330     C                   eval      wrkesito = '1'
018500010330     C                   else
018600010615     C                   if        flgOk = '0'
018700010615     C                   eval      wrkesito = '2'
018800010615     C                   else
018900010615     C                   eval      wrkesito = '6'
019000010615     C                   endif
019100010330     C                   endif
019200010330     C                   endif
019300010330     C*
019400010330     C                   if        %open(tivin00r)
019500010330     C                   close     tivin00r
019600010330     C                   endif
019700010330     C*
019800160512     c* Aggiornamento file filto
019900160512     c                   EXSR      AGGFILTO
020000111018     c
020100111018     c
020200010330     C                   ENDSR
020300010330     C***
020400010601
020500010330     C*----------------------------------------------------*
020600020722     C*  IMPOSTAZIONE CAMPI COSTANTI
020700010330     C*----------------------------------------------------*
020800010330     C     DEFCAM        BEGSR
020900010330     C*
021000020204     C* Inizializzo il buffer del record da scrivere
021100160512     C                   CLEAR                   FNORGDS
021200020204     C* Imposto i valori di default...
021300020204     C* ... e poi verifico se sono stati passati come parametri
021400020204     C                   IF        vlrppt > *blanks
021500060803     C                   IF        %trim(%subst(vlrppt:1:1)) = 'T'              * Esecuzione in test
021600060804     C                   EVAL      flgTest = 'S'
021700060803     C                   ENDIF
021800020204     C                   ENDIF
021900020204     C*
022000010330     C                   ENDSR
022100010607     C*----------------------------------------------------*
022200160512     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FNORG)
022300010607     C*----------------------------------------------------*
022400070112     C     IMPFILE       BEGSR
022500010607     C*
022600060905     C                   SETOFF                                       313233
022700060720     C*
022800060804     C* Ridefinisco i dati ricevuti da TELLUS
022900160512     C                   EVAL      dtlloi = vindta
023000060804     C*
023100160512     C* Valorizzazione buffer file FNORG
023200070201     c
023300070201     c* Verifico subito IDB per memorizzare la key di chain su FILTE
023400070201     c*  da aggiornare anche se tutti i record sono in errore blocante
023500160512    1c                   if        §tlloiIDB<>savtllIDB
023600070201     c                   clear                   erridb
023700070201     c                   clear                   pistr
023800160512     C                   EVAL      PiStr=§tlloiIDB
023900070201     C                   EXSR      CHKNUM
024000070201    2C                   IF        PiNum=*on
024100070201     C                   Z-ADD     PiVal         wIDB
024200070201     c
024300070201   x2C                   ELSE
024400070201     c* Errore non bloccante dati Bartolini
024500070201     c                   eval      erridb='E'
024600070201    2C                   ENDIF
024700070201     c
024800070201     c* Se non trovo l'ID elaborazione bartolini, errore non vincolante
024900070201    2c                   select
025000070201    2c                   when      widb>0 and keyidb>0 and widb<>keyidb
025100070201     c                   eval      erridb='D'
025200070201     c
025300070201     c* Errore non bloccante: idb diverso dal primo memorizzato
025400070201    2c                   when      widb>0 and keyidb=0
025500160512     c     widb          chain(N)  filto01l
025600160512    3c                   if        not %found(filto01l)
025700070201     c                   eval      erridb='N'
025800070201   x3c                   else
025900070201     c* salvo il campo key buono: non lo cambio +
026000070201     c                   eval      keyidb=widb
026100070201    3c                   endif
026200070201    2c                   endsl
026300070201     c
026400160512     c                   eval      savtllidb=§tlloiIDB
026500070201    1c                   endif
026600070124     c
026700070124     c* Se errore vincolante TELLUS, non carico il record
026800160512     c                   if        §tlloierr='02' or
026900160512     c                             §tlloierr='03' or
027000160512     c                             §tlloierr='04' or
027100160512     c                             §tlloierr='51' or
027200160512     c                             §tlloierr='52' or
027300160512     c                             §tlloierr='53'
027400070124     c                   goto      endimpo
027500070125     c                   endif
027600060905     c
027700160512     c* Ogni campo numerico in FNORG deve essere controllato prima di
027800060905     c*  essere mosso per evitare errori di dati decimali
027900060905     c
028000160512     c* Se elaborazione di TEST imposto 99 nella serie
028100060804     C                   IF        flgTest = 'S'
028200160512     C                   MOVEL     *all'9'       orgNSR
028300060804     C                   ELSE
028400060906     c                   clear                   pistr
028500160512     C                   EVAL      PiStr=§tlloiNSR
028600060905     C                   EXSR      CHKNUM
028700060905     C                   IF        PiNum=*on
028800160512     C                   Z-ADD     PiVal         orgnsr
028900060905     C                   ELSE
029000060905     c* Errore vincolante  dati Bartolini
029100060905     c                   seton                                        31
029200060905     C                   EVAL      vinmsg = %trimr(vinmsg)
029300160512     C                             + ' ' + 'ORGNSR'
029400060905     c                   goto      endimpo
029500060905     C                   ENDIF
029600060804     C                   ENDIF
029700060804     C*
029800060906     c                   clear                   pistr
029900160512     C                   EVAL      PiStr=§tlloiPOE
030000060905     C                   EXSR      CHKNUM
030100060905     C                   IF        PiNum=*on
030200160512     C                   Z-ADD     PiVal         orgPOE
030300060905     C                   ELSE
030400060905     c* Errore vincolante  dati Bartolini
030500060905     c                   seton                                        31
030600060905     C                   EVAL      vinmsg = %trimr(vinmsg)
030700160512     C                             + ' ' + 'ORGPOE'
030800060905     c                   goto      endimpo
030900060905     C                   ENDIF
031000060905     C*
031100060906     c                   clear                   pistr
031200160512     C                   EVAL      PiStr=§tlloiAAS
031300060905     C                   EXSR      CHKNUM
031400060905     C                   IF        PiNum=*on
031500160512     C                   Z-ADD     PiVal         waas              2 0
031600060905     C                   ELSE
031700160512     c* Errore NON vincolante  dati Bartolini (non ce l'ho su FNORG)
031800160512     c                   seton                                        32
031900060905     C                   EVAL      vinmsg = %trimr(vinmsg)
032000160512     C                             + ' ' + 'AAS'
032100060905     c                   goto      endimpo
032200060905     C                   ENDIF
032300060905     c
032400060906     c                   clear                   pistr
032500160512     C                   EVAL      PiStr=§tlloiNOR
032600060905     C                   EXSR      CHKNUM
032700060906    1C                   IF        PiNum=*on
032800160512     C                   Z-ADD     PiVal         orgNOR
032900060906   x1C                   ELSE
033000060905     c* Errore vincolante  dati Bartolini
033100060905     c                   seton                                        31
033200060905     C                   EVAL      vinmsg = %trimr(vinmsg)
033300160512     C                             + ' ' + 'ORGNOR'
033400060905     c                   goto      endimpo
033500060906    1C                   ENDIF
033600160512     c
033700160512     c                   clear                   pistr
033800160512     C                   EVAL      PiStr=§tlloiNRV
033900160512     C                   EXSR      CHKNUM
034000160512    1C                   IF        PiNum=*on
034100160512     C                   Z-ADD     PiVal         orgNRV
034200160512   x1C                   ELSE
034300160512     c* Errore vincolante  dati Bartolini
034400160512     c                   seton                                        31
034500160512     C                   EVAL      vinmsg = %trimr(vinmsg)
034600160512     C                             + ' ' + 'ORGNRV'
034700160512     c                   goto      endimpo
034800160512    1C                   ENDIF
034900160512     c* Verifico se esiste l'ORM    no per test
035000160512    0c                   if        orgnsr<>99
035100160512     c     korg          chain     fnorm01l
035200160512    1c                   if        not %found(fnorm01l)
035300060905     c* Errore vincolante  dati Bartolini
035400060905     c                   seton                                        31
035500060905     C                   EVAL      vinmsg = %trimr(vinmsg)
035600160512     C                             + ' ' + 'NUMORM'
035700060905     c                   goto      endimpo
035800060906   x1c                   else
035900160512 xx  C                   z-add     ormpor        orgPOR
036000060906     c
036100060906    1c                   endif
036200060906    0c                   endif
036300060905     c
036400060906     c                   clear                   pistr
036500160512     C                   EVAL      PiStr=§tlloiPOR
036600060905     C                   EXSR      CHKNUM
036700060905     C                   IF        PiNum=*on
036800160512     C                   Z-ADD     PiVal         orgPOR
036900080116     c
037000060906     c
037100060905     C                   ELSE
037200160512     c*  errore vincolante
037300080116     c                   seton                                        31
037400080116     C                   EVAL      vinmsg = %trimr(vinmsg)
037500160512     C                             + ' ' + 'ORGPOR'
037600080116     c                   goto      endimpo
037700080116     C                   ENDIF
037800060905     c
037900060907     c* Registro l'errore su tivin
038000060907     c                   if        erridb<>' '
038100060907     c                   seton                                        32
038200060907     c                   select
038300060907     c* camp non numerico
038400060907     c                   when      erridb='E'
038500060907     C                   EVAL      vinmsg = %trimr(vinmsg)
038600160512     C                             + ' ' + 'WIDB'
038700060907     c
038800060907     c                   when      erridb='D'
038900160512     c
039000060907     C                   EVAL      vinmsg = %trimr(vinmsg)
039100160512     C                             + ' ' + 'WIDB<>'
039200060907     c
039300060907     c                   when      erridb='N'
039400060907     c                   clear                   widb
039500070919     C                   EVAL      vinmsg = %trimr(vinmsg)
039600160512     C                             + ' ' + 'NoFILTO'
039700060907     c                   endsl
039800060907    2C                   ENDIF
039900060905     c
040000060905     c
040100060905     c* Se c'e' codice giro conto
040200160512     C                   MOVEL     §TLLOICGI     orgCGI
040300160512     c                   clear                   orgtgi
040400060905     c
040500070125     c
040600070125     c* Ora  elaborazione tellus
040700160512     c                   time                    orghvtel
040800070125     c* Data elaborazione tellus
040900070125     c                   time                    w0140            14 0
041000070125     c     *eur          move      w0140         dataeur
041100160512     c     *iso          movel     dataeur       orgdtvtel
041200020204     C*
041300060905     C     ENDimpo       ENDSR
041400010330
041500010330     C*----------------------------------------------------*
041600010330     C*  CONTROLLO NUMERICITA' CAMPI
041700010330     C*----------------------------------------------------*
041800010330     C     CHKNUM        BEGSR
041900010330     C*
042000010606     C                   IF        PiDecChr = *blanks
042100010606     C                   EVAL      PiDecChr = ','
042200010606     C                   ENDIF
042300010606     C*
042400010606     C                   CALL(e)   'ISNUMERIC'
042500010330     C                   PARM                    PiStr            30
042600010606     C                   PARM                    PiDecChr          1
042700010330     C                   PARM      *ZEROS        PiVal            30 9
042800010330     C                   PARM      '0'           PiInt             1
042900010330     C                   PARM      '0'           PiNum             1
043000010330     C                   IF        %error
043100010606     C                   EVAL      PiNum=*off
043200010330     C                   ENDIF
043300010330     C*
043400010330     C                   ENDSR
043500010330     C***
043600060906     C*----------------------------------------------------*
043700060906     C*  AGGIORNAMENTO FILE FILTE01L
043800060906     C*----------------------------------------------------*
043900160512     C     AGGFILTO      BEGSR
044000060907     c* Solo se trovato
044100060907     c                   if        keyidb>0
044200160512     c     keyidb        chain     filto01l
044300160512     c                   eval      ltonrig=contanrig
044400160512     c                   eval      ltonrir=contanrir
044500160512     c                   eval      ltonrieb=contanrieb
044600160512     c                   eval      ltonriet=contanriet
044700160512     c                   eval      ltonriew=contanriew
044800060906     c* Aggiorno campi di uoload
044900160512     c                   eval      ltoHDL=vlrhdl
045000160512     c                   eval      ltodatu=vlrdat
045100160512     c                   eval      ltoorau=vlrora
045200160512     c                   eval      ltoHDL=vlrhdl
045300080412     c*
045400160512     c                   update    filto000
045500080404     c*
045600060907     c                   endif
045700060907     c
045800060906     C                   ENDSR
045900070604
046000010601
046100060906     C*----------------------------------------------------*
046200060803      /TITLE Operazioni iniziali
046300060906     C*----------------------------------------------------*
046400010601     C     opeini        BEGSR
046500010601     C*
046600010601     C* Inizializzo flag e contatori operativi
046700010601     C                   movel     '0'           flgGiro           1
046800010615     C                   movel     '0'           flgOk             1
046900010601     C*
047000010601     C                   ENDSR
047100010601     C***
047200010330
047300060906     C*----------------------------------------------------*
047400000613     C     *inzsr        BEGSR
047500060906     C*----------------------------------------------------*
047600990910     C*
047700990910     C     *entry        plist
047800990920     C                   parm                    tivlrds
047900990921     C                   parm      wrkesito      esito
048000000724     C                   parm                    prmlit
048100000710     C                   parm                    prmfir
048200110701     c
048300060807     C*
048400060807     C* Definizione chiavi
048500060807     C*
048600160512     C* Chiave su FNORG01L - Completa
048700160512     C     KORG          KLIST
048800160512     C                   KFLD                    orgPOE
048900160512     C                   KFLD                    orgNSR
049000160512     C                   KFLD                    orgNOR
049100160512     C                   KFLD                    orgNRV
049200000613     C*
049300000613     C                   ENDSR
