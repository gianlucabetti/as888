000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600100412     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800111031     Ftitv2i3p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000110627     F                                     infsr(*pssr)
001100111031     Ftitv2i3ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300110627     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011113     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000801     D*----------------------------------------------------
002000000801     D* DICHIARAZIOINE VARIABILI DI WRK
002100000801     D*----------------------------------------------------
002200990920     D dscmz         e ds                  inz
002300990910     D psds           sds
002400990910     D  procname         *PROC
002500990920     D tivlrds       e ds                  extname(tivlr00f)
002600070730     D tisi95ds      e ds
002700990910     D esito           s              1
002800000724     D prmlit          s             10
002900000710     D prmfir          s             10
003000990921     D wrkesito        s                   like(esito)
003100990915     D wrkdata         s               d
003200990915     D wrkora          s               t
003300000613     D rrnum           s              6  0 INZ(*zeros)
003400000621     D recko           s            150    INZ(*blanks)
003500011113     D depcmd          s            150    INZ(*blanks)
003600111031     D depspe          s             25    INZ(*blanks)
003700111031     D curspe          s             25    INZ(*blanks)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004300091223     D Num5_0          s              5  0
004400020725
004500020725     D*------------------
004600020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004700020725     D*------------------
004800070530     D INPUT_DS        DS                  INZ
004900100729     D  VINDTA                     2048
005000100729     D  VINFLG                        1
005100111031     D  VINSPE                       25
005200100729     D  VINRRN                        8  0
005300020725     D*
005400081113
005500091223     D*------------------
005600091223     D* DS REPERIMENTO NUMERATORE
005700091223     D*------------------
005800100309     D trul33ds      e ds                  inz
005900091223     D*------------------
006000091223     D* DS ARCHITETTURA
006100091223     D*------------------
006200091223     D kpjba         e ds                  inz
006300091223
006400101119
006500101119     D*-------------------
006600101119     D* COSTANTI
006700101119     D*-------------------
006800101119     D minu            c                   const('qwertyuiopasdfghjklzxcvbnmìòà-    *alfabeto minus.
006900101119     D                                     èéù')
007000101119     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNMIOA-    *ALFABETO MAIUS.
007100101119     D                                     EEU')
007200081113
007300081113     D*------------------
007400081113     D* LINKING A DEFINIZIONI ESTERNE
007500081113     D*------------------
007600081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007700090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007800081113
007900990908
008000010201
008100010201
008200080117     C*
008300080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
008400080117     C
008500080117     C/EXEC SQL
008600080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008700080117     C/END-EXEC
008800080117     C
008900990915     C                   time                    wrkdata
009000990915     C                   time                    wrkora
009100000913     C                   reset                   rrnum
009200990921     C                   reset                   esito
009300990921     C                   reset                   wrkesito
009400000724     C*
009500000724     C* SE OCCORRE SPEDIRE IN FILIALE
009600110411     C                   if        vlrpoi <> *zeros AND vlrpoi <> 999
009700000724     C*
009800000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009900000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010000000724     C     vlrpoi        chain     azorg01l
010100000724     C                   if        %found
010200000616     C                   movel(p)  CMD(1)        depcmd
010300020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010400000616     C*
010500000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010600110627     C                   Z-ADD     150           LENGH            15 5
010700000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010800000616     C                   PARM                    depcmd
010900000616     C                   PARM                    LENGH
011000000724     C*
011100000724     C                   endif
011200000724     C                   endif
011300000616     C*
011400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011500000616     C                   movel(p)  CMD(2)        depcmd
011600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011700000616     C*
011800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011900110627     C                   Z-ADD     150           LENGH            15 5
012000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012100000616     C                   PARM                    depcmd
012200000616     C                   PARM                    LENGH
012300000616     C*
012400111031     C                   if        not %open(titv2i3ps)
012500111031     C                   open      titv2i3ps
012600000616     C                   except    testdett
012700000613     C                   endif
012800000613     C*
012900100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
013000070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
013100110411     C                   if        vlrpoi <> *zeros AND vlrpoi <> 999
013200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
013300110411     C                   endif
013400000613     C*
013500010202     C* Effettuo la chiamata al CLLE preposto
013600100525     C  N51              call(e)   'TITVVTC'
013700100525     C                   parm                    parccm
013800100525     C                   parm                    parmbr
013900100525     C                   parm      '2'           paropz
014000100525     C   51              call(e)   'TITVVBC'
014100100525     C                   parm                    parccm
014200100525     C                   parm                    parmbr
014300100525     C                   parm      '2'           paropz
014400070730     C*
014500070730     C* Effettuo lancio TISI95 solo x chiusura
014600070730     C                   CLEAR                   TISI95DS
014700070730     C                   EVAL      I95TLA = 'C'
014800070730     C                   CALL      'TISI95R'
014900070730     C                   PARM                    TISI95DS
015000010202     C*
015100111031     C                   if        %open(titv2i3ps)
015200000616     C                   except    findett
015300111031     C                   close     titv2i3ps
015400000613     C                   endif
015500000616     C*
015600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015700110411     C                   if        vlrpoi <> *zeros AND vlrpoi <> 999
015800110627     C                   Z-ADD     100           LENGH            15 5
015900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016000000616     C                   PARM                    CMD(3)
016100000616     C                   PARM                    LENGH
016200110411     C                   endif
016300110411     C*
016400110411     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016500110627     C                   Z-ADD     100           LENGH            15 5
016600110411     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016700110411     C                   PARM                    CMD(4)
016800110411     C                   PARM                    LENGH
016900000616     C*
017000000801     C
017100010201     C                   seton                                        LR
017200000613
017300100310
017400100310
017500000613     C*--------------------------------------------------------
017600000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
017700000613     C*--------------------------------------------------------
017800000621     C     STPR          BEGSR
017900000613     C*
018000111031     C                   if        not %open(titv2i3p)
018100111031     C                   open      titv2i3p
018200990915     C                   endif
018300990915     C*
018400110411     C                   except    riepilogo
018500990915     C*
018600111031     C                   if        %open(titv2i3p)
018700111031     C                   close     titv2i3p
018800990914     C                   endif
018900990910     C*
019000000613     C                   ENDSR
019100000613     C***
019200990908
019300000801
019400910830     C*--------------------------------------------------------
019500070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
019600910830     C*--------------------------------------------------------
019700070530     C     RWFILE        BEGSR
019800990910     C*
019900990914     C                   if        not %open(tivin00r)
020000990908     C                   open      tivin00r
020100990914     C                   endif
020200070530     C*
020300100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
020400100525     C                   exsr      prevasx
020500010201     C*
020600010202     C                   if        chkcall = '0'
020700010202     C*
020800100525     C                   if        not %open(fivabwwr)
020900100525     C                   open      fivabwwr
021000100525     C                   endif
021100100525     C*
021200021113     C                   if        not %open(fivatwwr)
021300021113     C                   open      fivatwwr
021400010201     C                   endif
021500080117     C*
021600080117     C                   EXSR      INZVAR
021700080117     C                   EXSR      DEFCAM
021800990910     C*
021900010201     C                   clear                   §CTROKVB          5 0
022000020305     C                   clear                   §CTROKVT          5 0
022100000801     C                   clear                   §CTRMO            5 0
022200000801     C                   clear                   §CTRNO            5 0
022300990910     C*
022400020725     C/EXEC SQL
022500020725     C+ declare C1 cursor for select
022600111031     C+ vindta, vinflg, substr(vindta, 46, 25) as sped, rrn(tivin00r)
022700060223     C+ from tivin00r
022800111020     C+ order by sped
022900020725     C+ for read only
023000020725     C/END-EXEC
023100020725     C
023200020725     C/EXEC SQL
023300020725     C+ open C1
023400020725     C/END-EXEC
023500020725     C
023600020725     C/EXEC SQL
023700070530     C+ Fetch C1 into :INPUT_DS
023800020725     C/END-EXEC
023900020725     C*
024000020725     C                   dow       sqlcod = *zeros
024100990913     C*
024200100310     C                   if        vindta > *blanks
024300000613     C                   add       1             rrnum
024400000801     C*
024500020725     C                   if        vinflg = *blanks
024600020725     C                             or vinflg = '0'
024700020725     C                             or vinflg = '2'
024800000801     C*
024900020725     C                   clear                   x_vinmsg
025000020725     C                   eval      x_vinflg = '1'
025100101119     C*
025200101119     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
025300101119     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
025400010305     C*
025500010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
025600111031     C                   EVAL      curspe=%trim(%subst(vindta:46:25))
025700010305     C*
025800100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
025900080923     C                   if        *in50 = *off
026000120921     C*
026100120921     C* Se nn richiesta esclusione spedizioni "duplicate"
026200120921     C                   select
026300120921     C                   when      wDUPREC = ' '
026400101119     C                   exsr      inzvar
026500101119     C                   exsr      defcam
026600120921     C                   exsr      repNSP
026700071003     C                   exsr      impvab
026800100525     C                   exsr      wrivab
026900111031     C                   exsr      exevata                                      => carico VAT
027000111031     C                   exsr      exevatb                                      => carico VAT
027100111031     C   51              exsr      exevate                                      => carico VAT
027200120921     C                   when      wDUPREC = 'T'
027300120921     C                   if        depspe = *blanks                             => 1° giro
027400120921     C                   eval      depspe = curspe                              => memorizz. spediz
027500120921     C                   exsr      inzvar
027600120921     C                   exsr      defcam
027700120921     C                   exsr      repNSP
027800120921     C                   exsr      impvab
027900120921     C                   exsr      wrivab
028000120921     C                   exsr      exevata                                      => carico VAT
028100120921     C                   exsr      exevatb                                      => carico VAT
028200120921     C   51              exsr      exevate                                      => carico VAT
028300120921     C                   else
028400120921     C                   if        depspe <> curspe                             => rottura di spediz
028500120921     C                   eval      depspe = curspe                              => memorizz. spediz
028600120921     C                   exsr      inzvar
028700120921     C                   exsr      defcam
028800120921     C                   exsr      repNSP
028900120921     C                   exsr      impvab
029000120921     C                   exsr      wrivab
029100120921     C                   exsr      exevata                                      => carico VAT
029200120921     C                   exsr      exevatb                                      => carico VAT
029300120921     C   51              exsr      exevate                                      => carico VAT
029400120921     C                   else
029500120921     C   51              exsr      exevate                                      => carico VAT
029600120921     C                   endif                                                  => x stessa spediz
029700120921     C                   endif                                                  => x stessa spediz
029800120921     C                   endsl
029900120921     C*
030000071003     C                   else
030100080923     C*
030200071009     C                   if        wDISK = 'D'
030300091223     C                   exsr      repNSP
030400071009     C                   exsr      impvab
030500100412     C                   exsr      wrivab
030600111031     C                   exsr      exevata                                      => carico VAT
030700111031     C                   exsr      exevatb                                      => carico VAT
030800111031     C                   exsr      exevate                                      => carico VAT
030900071009     C                   else
031000071009     C*
031100010305     C                   if        depspe = *blanks                             => 1° giro
031200010305     C                   eval      depspe = curspe                              => memorizz. spediz
031300080117     C                   clear                   tivinds
031400091223     C                   exsr      repNSP
031500020305     C                   exsr      impvab
031600111031     C                   exsr      exevata                                      => carico VAT
031700111031     C                   exsr      exevatb                                      => carico VAT
031800111031     C   50              exsr      exevate                                      => carico VAT
031900010305     C                   else
032000020725     C                   if        depspe <> curspe                             => rottura di spediz
032100010305     C                   eval      depspe = curspe                              => memorizz. spediz
032200100412     C                   exsr      wrivab
032300080117     C                   clear                   tivinds
032400091223     C                   exsr      repNSP
032500020305     C                   exsr      impvab
032600111031     C                   exsr      exevata                                      => carico VAT
032700111031     C                   exsr      exevatb                                      => carico VAT
032800111031     C   50              exsr      exevate                                      => carico VAT
032900020305     C                   else                                                   => x stessa spediz
033000100401     C*
033100100401     C* Se nn richiesta esclusione spedizioni "duplicate"
033200100928     C                   select
033300100928     C                   when      wDUPREC = ' '
033400020305     C                   exsr      impvab
033500111031     C                   exsr      exevata                                      => carico VAT
033600111031     C                   exsr      exevatb                                      => carico VAT
033700111031     C   50              exsr      exevate                                      => carico VAT
033800100928     C                   when      wDUPREC = 'C'
033900111031     C   50              exsr      exevate                                      => carico VAT
034000100928     C                   endsl
034100010305     C                   endif
034200010305     C                   endif
034300010305     C                   endif
034400071003     C                   endif
034500110201     C*
034600071009     C                   endif
034700000905     C*
034800000905     C                   else
034900020725     C                   eval      x_vinflg = '1'
035000000905     C                   endif
035100000905     C*
035200020725     C     VINRRN        chain     tivin000
035300020725     C                   if        %found(tivin00r)
035400020725     C                   eval      y_vinflg = x_vinflg
035500020725     C                   eval      y_vinmsg = x_vinmsg
035600020725     C                   update    tivin000
035700020725     C                   endif
035800020725     C*
035900020725     C/EXEC SQL
036000070530     C+ Fetch C1 into :INPUT_DS
036100020725     C/END-EXEC
036200020725     C*
036300020725     C                   enddo
036400020725     C*
036500020725     C/EXEC SQL
036600020725     C+ close C1
036700020725     C/END-EXEC
036800000905     C*
036900020305     C* Scarico i VAB rimasti "in sospeso"
037000071009     C                   if        wDISK = 'C'
037100100412     C                   exsr      wrivab
037200071009     C                   endif
037300010202     C*
037400010202     C                   endif
037500990910
037600990910     C* Se non ci sono record con errori ...
037700000710     C                   if        §ctrno = 0
037800990910     C* ... restituisco esito OK.
037900990921     C                   eval      wrkesito = '0'
038000990910     C                   else
038100100525     C                   if        §ctrokvb > 0 OR
038200100525     C                             §ctrokvt > 0
038300990921     C                   eval      wrkesito = '1'
038400000710     C                   else
038500000710     C                   eval      wrkesito = '2'
038600990910     C                   endif
038700000710     C                   endif
038800990910     C*
038900990914     C                   if        %open(tivin00r)
039000990908     C                   close     tivin00r
039100990914     C                   endif
039200021113     C                   if        %open(fivabwwr)
039300021113     C                   close     fivabwwr
039400990914     C                   endif
039500021113     C                   if        %open(fivatwwr)
039600021113     C                   close     fivatwwr
039700010201     C                   endif
039800990910     C*
039900100525     C                   if        §ctrokvb > 0 OR
040000100525     C                             §ctrokvt > 0
040100000724     C                             and vlrpoi <> *zeros
040200010202     C                   exsr      invio
040300990920     C                   endif
040400990920     C*
040500910830     C                   ENDSR
040600000613     C***
040700100412
040800100412
040900010305
041000010305     C*----------------------------------------------------*
041100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
041200010305     C*----------------------------------------------------*
041300020305     C     WRIVAB        BEGSR
041400100412     C*
041500100412     C* Gestisco l'eventuale rottura x numero spedizione
041600100412     C                   if        VABRMA = *blanks
041700100412     C                   movel(P)  VABRMN        VABRMA
041800100412     C                   endif
041900100412     C*
042000100525     C                   if        *in51 = *off and *in31 = *off
042100100525     C                   write     fivab000                                     => scarico il VAB
042200100525     C                   endif
042300010305     C*
042400010305     C                   ENDSR
042500990920
042600000801     C*----------------------------------------------------*
042700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
042800000801     C*----------------------------------------------------*
042900010201     C     INZVAR        BEGSR
043000000801     C*
043100010201     C                   Z-ADD     *zeros        Num5_0
043200020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
043300020725     C                   MOVEL     '0'           FlgCAS            1
043400000801     C*
043500000801     C                   ENDSR
043600000801     C*----------------------------------------------------*
043700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
043800000801     C*----------------------------------------------------*
043900020904     C     DEFCAM        BEGSR
044000080923     C*
044100100928     C                   SETOFF                                       505160
044200000801     C*
044300021113     C                   CLEAR                   FIVAB000
044400021113     C                   CLEAR                   FIVAT000
044500070730     C* Imposto i valori di default...
044600111031     C                   EVAL      VABCCM = 0434461
044700111031     C                   EVAL      VATCCM = 0434461
044800111031     C                   EVAL      VABLNP = 043
044900111031     C                   EVAL      VATLNP = 043
045000111031     C                   EVAL      VABCTR = 000
045100100525     C                   EVAL      VABCBO = '1'
045200070222     C* ... e poi verifico se sono stati passati come parametri
045300070222     C                   IF        vlrppt > *blanks
045400100525     C*
045500070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
045600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
045700070222     C                   EXSR      CHKNUM
045800070222     C                   IF        PiInt=*on
045900070222     C                   Z-ADD     PiVal         VABCCM
046000070222     C                   Z-ADD     PiVal         VATCCM
046100070222     C                   ENDIF
046200070222     C                   ENDIF
046300070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
046400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
046500070222     C                   EXSR      CHKNUM
046600070222     C                   IF        PiInt=*on
046700070222     C                   Z-ADD     PiVal         VABLNP
046800070222     C                   Z-ADD     PiVal         VATLNP
046900070222     C                   ENDIF
047000070222     C                   ENDIF
047100070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
047200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
047300070222     C                   EXSR      CHKNUM
047400070222     C                   IF        PiInt=*on
047500070222     C                   Z-ADD     PiVal         VABCTR
047600070222     C                   ENDIF
047700070928     C                   ENDIF
047800071009     C                   MOVEL     *blanks       wDISK             1
047900071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
048000071009     C                   IF        wDISK <> *blanks
048100070928     C                   SETON                                        50
048200070222     C                   ENDIF
048300100525     C                   IF        wDISK  = 'T'
048400100525     C                   SETOFF                                       50
048500100525     C                   SETON                                        51
048600100525     C                   ENDIF
048700100401     C                   MOVEL     *blanks       wDUPREC           1
048800100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
048900100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
049000100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
049100100525     C                   ENDIF
049200111031     C                   MOVEL     *blanks       wNCL              1            1/A/*blanks
049300111031     C                   EVAL      wNCL = %subst(vlrppt:18:1)
049400100525     C*
049500070222     C                   ENDIF
049600071009     C*
049700071009     C   50              EVAL      VABCTM = '7Q'
049800000801     C*
049900000801     C                   ENDSR
050000091223     C*----------------------------------------------------*
050100091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
050200091223     C*----------------------------------------------------*
050300091223     C     REPNSP        BEGSR
050400091223     C*
050500091223     C                   EXSR      INZVAR
050600091223     C                   EXSR      DEFCAM
050700100928     C*
050800100928     C                   SETON                                        60
050900091223     C*
051000091223     C* NSP => Stacco un numeratore da AZNUM
051100091223     C                   clear                   TRUL33DS
051200091223     C                   eval      I33OPE = *zeros
051300091223     C                   eval      I33CNU = 302
051400091223     C                   eval      I33NUM = 1
051500091223     C                   movel     TRUL33DS      KPJBU
051600091223     C                   call      'TRUL33R'
051700091223     C                   parm                    KPJBA
051800091223     C                   movel     KPJBU         TRUL33DS
051900091223     C                   if        O33ERR = *zeros
052000091223     C                   z-add     O33NRF        VABNSP
052100091223     C                   z-add     O33NRF        VATNSP
052200091223     C                   else
052300091223     C                   SETON                                        31
052400091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052500091223     C                             + ' ' + 'VABNSP VATNSP'
052600091223     C                   endif
052700091223     C*
052800091223     C                   ENDSR
052900111031     C*----------------------------------------------------*
053000021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
053100111031     C*----------------------------------------------------*
053200111031     C     IMPVAB        BEGSR
053300111031     C*
053400111031     C                   Z-ADD     *zeros        errore            1 0
053500111031     C                   MOVEL     datcor        VABAAS
053600111031     C                   MOVEL     datcor        VATAAS
053700111031     C                   MOVE      datcor        VABMGS
053800111031     C                   MOVE(P)   vlrpoi        VABFGS
053900111031     C                   MOVE(P)   vlrpoi        VATFGS
054000111031     C*
054100111031     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
054200111031     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
054300111031     C     '@':'A'       XLATE     VABRSD        VABRSD
054400111031     C* ==
054500111031     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
054600111031     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
054700111031     C                                    %trim(%subst(vindta:341:30))+' '+
054800111031     C                                    %trim(%subst(vindta:371:5)))
054900111031     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
055000111031     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
055100111031     C***                EVAL      VABRMA=%trim(%subst(vindta:46:10))
055200111031     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
055300111031     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
055400111031     C                   IF        %subst(vindta:43:1) = 'N'
055500111031     C                   EVAL      VABCBO='1'
055600111031     C                   ENDIF
055700111031     C                   IF        %subst(vindta:43:1) = 'P'
055800111031     C                   EVAL      VABCBO='2'
055900111031     C                   ENDIF
056000111031     C                   IF        %subst(vindta:511:3) = 'ABM'
056100111031     C                   EVAL      VABTIC='BM'
056200111031     C                   ENDIF
056300111031     C                   IF        %subst(vindta:511:3) = 'ABS'
056400111031     C                   EVAL      VABTIC='B'
056500111031     C                   ENDIF
056600111031     C                   IF        %subst(vindta:511:3) = 'CON'
056700111031     C                   EVAL      VABTIC=*blanks
056800111031     C                   ENDIF
056900111031     C                   IF        %subst(vindta:511:3) = 'ACM'
057000111031     C                   EVAL      VABTIC='CM'
057100111031     C                   ENDIF
057200111031     C                   IF        %subst(vindta:511:3) = 'ACS'
057300111031     C                   EVAL      VABTIC='CB'
057400111031     C                   ENDIF
057500111031     C                   IF        %subst(vindta:511:3) = 'VAR'
057600111031     C                   EVAL      VABTIC=*blanks
057700111031     C                   ENDIF
057800111031     C* CAP
057900111031     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
058000111031     C                   EXSR      CHKNUM
058100111031     C                   IF        PiInt=*on
058200111031     C                   Z-ADD     PiVal         Num5_0
058300111031     C                   MOVEL(P)  Num5_0        VABCAD
058400111031     C                   ELSE
058500111031     C                   ADD       1             errore
058600111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058700111031     C                             + ' ' + 'VABCAD'
058800111031     C                   ENDIF
058900111031     C* Reperisco la provincia dal CAP e dalla località
059000111031     C                   IF        VABCAD <> *blanks AND
059100111031     C                             VABPRD  = *blanks
059200111031     C                   CLEAR                   TISI95DS
059300111031     C                   EVAL      I95TCN = '3'
059400111031     C                   Z-ADD     datcor        I95DAT
059500111031     C                   EVAL      I95CAP = VABCAD
059600111031     C                   EVAL      I95LOC = VABLOD
059700111031     C                   EVAL      I95NAR = VABNZD
059800111031     C                   CALL      'TISI95R'
059900111031     C                   PARM                    TISI95DS
060000111031     C                   EVAL      VABPRD = O95PRV
060100111031     C                   ENDIF
060200111031     C* NCL
060300111031     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
060400111031     C                   EXSR      CHKNUM
060500111031     C                   IF        PiInt=*on
060600111031     C                   SELECT
060700111031     C                   WHEN      wNCL = *blanks
060800111031     C                   Z-ADD     PiVal         VABNCL
060900111031     C                   WHEN      wNCL = 'A'
061000111031     C                   ADD       PiVal         VABNCL
061100111031     C                   WHEN      wNCL = '1'
061200111031     C                   ADD       1             VABNCL
061300111031     C                   ENDSL
061400111031     C                   ELSE
061500111031     C                   ADD       1             errore
061600111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
061700111031     C                             + ' ' + 'VABNCL'
061800111031     C                   ENDIF
061900111031     C* PKB
062000111031     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
062100111031     C                   EXSR      CHKNUM
062200111031     C                   IF        PiNum=*on
062300111031     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
062400111031     C                   Z-ADD     PiVal         VABPKB
062500111031     C                   ELSE
062600111031     C                   ADD       1             errore
062700111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
062800111031     C                             + ' ' + 'VABPKB'
062900111031     C                   ENDIF
063000111031     C* RMN
063100111031     C***                EVAL      PiStr=%trim(%subst(vindta:46:25))
063200111031     C                   EVAL      PiStr=%trim(%subst(vindta:46+10:15))
063300111031     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
063400111031     C***                EVAL      PiStr=%trim(%subst(vindta:47:24))
063500111031     C                   EXSR      CHKNUM
063600111031     C                   IF        PiInt=*on
063700111031     C                   Z-ADD     PiVal         VABRMN
063800111031     C                   ELSE
063900111031     C                   ADD       1             errore
064000111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
064100111031     C                             + ' ' + 'VABRMN'
064200111031     C                   ENDIF
064300111031     C* CAS
064400111031     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
064500111031     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
064600111031     C                             %trim(%subst(vindta:502:9))<>*zeros
064700111031     C                   EVAL      VABVCA = 'EUR'
064800120315     C                   EVAL      VABTIC = 'TM'
064900111031     C                   EVAL      FlgCAS = '1'
065000111031     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
065100111031     C                   EXSR      CHKNUM
065200111031     C                   IF        PiNum=*on
065300111031     C                   Z-ADD     PiVal         VABCAS
065400111031     C                   ELSE
065500111031     C                   ADD       1             errore
065600111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
065700111031     C                             + ' ' + 'VABCAS'
065800111031     C                   ENDIF
065900111031     C                   ENDIF
066000111031     C* IAS
066100111031     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
066200111031     C                   EVAL      VABVAS = 'EUR'
066300111031     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
066400111031     C                   EXSR      CHKNUM
066500111031     C                   IF        PiNum=*on
066600111031     C                   Z-ADD     PiVal         VABIAS
066700111031     C                   ELSE
066800111031     C                   ADD       1             errore
066900111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
067000111031     C                             + ' ' + 'VABIAS'
067100111031     C                   ENDIF
067200111031     C                   ENDIF
067300111031     C*
067400111031     C* Considerazioni sul contenuto di campi precedentemente valorizzati
067500111031     C                   IF        FlgCAS <> '0'
067600111031     C                   IF        VABCBO = '1'
067700111031     C                   EVAL      VABCBO = '4'
067800111031     C                   ELSE
067900111031     C                   EVAL      VABCBO = '6'
068000111031     C                   ENDIF
068100111031     C                   ENDIF
068200111031     C*
068300111031     C* Eseguo routine finale x considerazioni specifiche su importi/divise
068400111031     C                   EXSR      CHKIMPDIV
068500111031     C*
068600111031     C* Ebbene...
068700111031     C                   ADD       1             §CTRMO
068800111031     C                   IF        errore <> *zeros
068900111031     C                   ADD       1             §CTRNO
069000111031     C                   EVAL      recko = vindta
069100111031     C                   EXCEPT    rigadett
069200111031     C                   EVAL      vinflg = '2'
069300111031     C                   ELSE
069400111031     C                   ADD       1             §CTROKVB
069500111031     C                   ENDIF
069600111031     C*
069700111031     C                   ENDSR
069800111031
069900111031
070000111031     C*----------------------------------------------------*
070100111031     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
070200111031     C*----------------------------------------------------*
070300111031     C     CHKIMPDIV     BEGSR
070400111031     C*
070500111031     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
070600111031     C                   Z-ADD     *zeros        wrkDec            9 9
070700111031     C*
070800111031     C* Come prima cosa effettuo considerazioni sulla divisa
070900111031     C                   IF        vabIAS > *zeros
071000111031     C                   IF        vabVAS <> 'EUR'
071100111031     C                   EVAL      vabVAS =  'ITL'
071200111031     C                   ENDIF
071300111031     C                   ENDIF
071400111031     C*
071500111031     C                   IF        vabCAS > *zeros
071600111031     C                   IF        vabVCA <> 'EUR'
071700111031     C                   EVAL      vabVCA =  'ITL'
071800111031     C                   ENDIF
071900111031     C                   ENDIF
072000111031     C*
072100111031     C                   IF        vabVMD > *zeros
072200111031     C                   IF        vabVAD <> 'EUR'
072300111031     C                   EVAL      vabVAD =  'ITL'
072400111031     C                   ENDIF
072500111031     C                   ENDIF
072600111031     C*
072700111031     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
072800111031     C                   Z-ADD     vabIAS        wrkDec
072900111031     C                   IF        wrkDec > *zeros
073000111031     C                   IF        vabVAS = 'ITL'
073100111031     C                   EVAL      vabIAS = *zeros
073200111031     C                   ENDIF
073300111031     C                   ENDIF
073400111031     C*
073500111031     C* Stabilisco se il contrasegno ha decimali valorizzati
073600111031     C                   Z-ADD     vabCAS        wrkDec
073700111031     C                   IF        wrkDec > *zeros
073800111031     C                   IF        vabVCA = 'ITL'
073900111031     C                   EVAL      vabCAS = *zeros
074000111031     C                   ENDIF
074100111031     C                   ENDIF
074200111031     C*
074300111031     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
074400111031     C                   Z-ADD     vabVMD        wrkDec
074500111031     C                   IF        wrkDec > *zeros
074600111031     C                   IF        vabVAD = 'ITL'
074700111031     C                   EVAL      vabVMD = *zeros
074800111031     C                   ENDIF
074900111031     C                   ENDIF
075000111031     C*
075100111031     C                   ENDSR
075200111031     C***
075300111031
075400111031
075500111031     C*----------------------------------------------------*
075600111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
075700111031     C*----------------------------------------------------*
075800111031     C     EXEVATA       BEGSR
075900111031     C*
076000111031     C***                EVAL      VATTRC='A'
076100111031     C***                EVAL      VATNOT = %trim(%subst(vindta:581:30))
076200111031     C***                exsr      wrivat                                       => scarico VAT
076300111031     C*
076400111031     C                   ENDSR
076500111031     C*----------------------------------------------------*
076600111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
076700111031     C*----------------------------------------------------*
076800111031     C     EXEVATB       BEGSR
076900111031     C*
077000111031     C***                EVAL      VATTRC='B'
077100111031     C***                EVAL      VATNOT = %trim(%subst(vindta:376:15))
077200111031     C***                exsr      wrivat                                       => scarico VAT
077300111031     C*
077400111031     C                   ENDSR
077500111031     C*----------------------------------------------------*
077600111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
077700111031     C*----------------------------------------------------*
077800111031     C     EXEVATE       BEGSR
077900111031     C*
078000111031     C                   EVAL      VATTRC='E'
078100111031     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
078200111031     C                   exsr      wrivat                                       => scarico VAT
078300111031     C*
078400111031     C                   ENDSR
078500111031     C*----------------------------------------------------*
078600111031     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
078700111031     C*----------------------------------------------------*
078800111031     C     WRIVAT        BEGSR
078900111031     C*
079000111031     C* Scrivo solo se valorizzato qualcosa
079100111031     C                   IF        VATNOT <> *blanks
079200111031     C                   WRITE     FIVAT000
079300111031     C                   ENDIF
079400111031     C*
079500111031     C                   ENDSR
079600020904
079700020904
079800010202     C*----------------------------------------------------*
079900100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
080000010202     C*----------------------------------------------------*
080100100525     C     PREVASX       BEGSR
080200100525     C*
080300021113     C* Compongo il nome del membro da dare al FIVATWWR
080400010202     C                   eval      parmbr = vlrhdl
080500010202     C                   movel     'M'           parmbr
080600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
080700010202     C                   eval      paropz = '1'
080800100525     C*
080900010202     C* Effettuo la chiamata al CLLE preposto
081000100525     C  N51              call(e)   'TITVVTC'
081100010202     C                   parm                    parccm
081200010202     C                   parm                    parmbr
081300010202     C                   parm                    paropz
081400100525     C   51              call(e)   'TITVVBC'
081500100525     C                   parm                    parccm
081600100525     C                   parm                    parmbr
081700100525     C                   parm                    paropz
081800100525     C*
081900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
082000010202     C                   if        %error
082100010202     C                   movel     '1'           chkcall
082200010202     C                   else
082300010202     C                   movel     '0'           chkcall
082400010202     C                   endif
082500010202     C*
082600010202     C                   ENDSR
082700000801     C*----------------------------------------------------*
082800000801     C*  CONTROLLO NUMERICITA' CAMPI
082900000801     C*----------------------------------------------------*
083000000801     C     CHKNUM        BEGSR
083100081113     C*
083200081113     C                   IF        PiDecChr = *blanks
083300110408     C                   EVAL      PiDecChr = '.'
083400081113     C                   ENDIF
083500091223     C*
083600081113     C                   callp(e)  UBISNUM_Check(PiStr
083700081113     C                                          :PiDecChr
083800081113     C                                          :PiVal
083900081113     C                                          :PiNum
084000081113     C                                          :PiInt)
084100081113     C*
084200000801     C                   IF        %error
084300000801     C                   EVAL      PiInt=*off
084400000801     C                   ENDIF
084500000801     C*
084600000801     C                   ENDSR
084700000801     C***
084800011113
084900011113
085000000801
085100000801
085200990920      /TITLE Invio dei dati al punto operativo.
085300010202     C     invio         BEGSR
085400990920     C*
085500021113     C* 1° invio FIVAT
085600010201     C                   reset                   dscmz
085700110411     C                   move      vatfgs        cmzdst
085800021113     C                   eval      cmzfld = 'FIVATWWR'
085900010201     C                   eval      cmzmbd = vlrhdl
086000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
086100021009     C***                if        prmfir = *blanks
086200021113     C                   eval      cmzfla = 'FIVAT00F'
086300021113     C                   eval      cmzmba = 'FIVAT00F'
086400021009     C***                else
086500021009     C***                eval      cmzfla = prmfir
086600021009     C***                eval      cmzmba = prmfir
086700021009     C***                endif
086800010201     C                   eval      cmznrr = *zeros
086900020305     C                   move      §ctrokvt      cmznrr
087000021018     C                   eval      cmzlba = vlrfl1
087100010201     C                   call(e)   'TIS711C'
087200010201     C                   parm                    dscmz
087300010201     C                   parm      *blanks       esito
087400010205     C                   if        %error
087500010205     C                             or cmzerr = '1'
087600010205     C                             or esito  = '1'
087700010205     C                   eval      wrkesito = '3'
087800010205     C                   else
087900010201     C*
088000021113     C* 2° invio FIVAB
088100100525     C                   if        *in51 = *off
088200010201     C                   reset                   dscmz
088300110411     C                   move      vabfgs        cmzdst
088400010201     C                   eval      cmzfld = vlrfou
088500010201     C                   eval      cmzmbd = vlrhdl
088600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
088700021009     C***                if        prmfir = *blanks
088800021113     C                   eval      cmzfla = 'FIVAB00F'
088900021113     C                   eval      cmzmba = 'FIVAB00F'
089000021009     C***                else
089100021009     C***                eval      cmzfla = prmfir
089200021009     C***                eval      cmzmba = prmfir
089300021009     C***                endif
089400010201     C                   eval      cmznrr = *zeros
089500010201     C                   move      §ctrokvb      cmznrr
089600021018     C                   eval      cmzlba = vlrfl1
089700010201     C                   call(e)   'TIS711C'
089800010201     C                   parm                    dscmz
089900010201     C                   parm      *blanks       esito
090000010201     C                   if        %error
090100010201     C                             or cmzerr = '1'
090200010201     C                             or esito  = '1'
090300010201     C                   eval      wrkesito = '3'
090400010201     C                   endif
090500100525     C                   endif
090600010205     C                   endif
090700990920     C*
090800000613     C                   ENDSR
090900000613     C***
091000070411
091100110627     C     *pssr         BEGSR
091200070411     C*
091300110627     C                   if        %open(tivin00r)
091400110627     C                   close     tivin00r
091500110627     C                   endif
091600110627     C                   if        %open(fivabwwr)
091700110627     C                   close     fivabwwr
091800110627     C                   endif
091900110627     C                   if        %open(fivatwwr)
092000110627     C                   close     fivatwwr
092100110627     C                   endif
092200110627     C*
092300110627     C* Effettuo la chiamata al CLLE preposto
092400110627     C  N51              call(e)   'TITVVTC'
092500110627     C                   parm                    parccm
092600110627     C                   parm                    parmbr
092700110627     C                   parm      '2'           paropz
092800110627     C   51              call(e)   'TITVVBC'
092900110627     C                   parm                    parccm
093000110627     C                   parm                    parmbr
093100110627     C                   parm      '2'           paropz
093200110627     C*
093300110627     C                   eval      wrkesito = '2'
093400110627     C*
093500110627     C                   seton                                        LR
093600110627     C*
093700110627     C                   ENDSR     '*CANCL'
093800070411     C***
093900070411
094000990910
094100000613     C     *inzsr        BEGSR
094200990910     C*
094300990910     C     *entry        plist
094400990920     C                   parm                    tivlrds
094500990921     C                   parm      wrkesito      esito
094600000724     C                   parm                    prmlit
094700000710     C                   parm                    prmfir
094800000613     C*
094900000830     C* CALCOLA LA DATA CORRENTE
095000091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
095100091223     C                   eval      datcor = %dec(%date() : *ISO)
095200000830     C*
095300000613     C                   ENDSR
095400000613     C***
095500990908
095600111031     Otitv2i3p  E            riepilogo         2
095700990915     O                                              'Upload via Internet'
095800990915     O                                           +1 'Traduzione TIVIN00R -'
095900021113     O                                           +1 'FIVABWWR/FIVATWWR'
096000010201     O                                           +1 'Report testate bolle'
096100990915     O          E            riepilogo   2
096200990915     O                       wrkdata
096300990915     O                       wrkora              +1
096400990915     O                       procname            +1
096500990915     O          E            riepilogo   2
096600990915     O                                              'Cliente..................:'
096700990915     O                       VABCCM        z     +1
096800990915     O          E            riepilogo   2
096900990920     O                                              'Riferimento Strategi.....:'
097000990920     O                       vlrhdl              +1
097100990915     O          E            riepilogo   2
097200990915     O                                              'Giusti...................:'
097300010201     O                       §CTROKVB      2   +  1
097400100525     O                       §CTROKVT      2   +  1
097500990915     O          E            riepilogo   2
097600990915     O                                              'Sbagliati e corretti.....:'
097700971022     O                       §CTRMO        2   +  1
097800990915     O          E            riepilogo   2
097900990915     O                                              'Sbagliati e scartati.....:'
098000971022     O                       §CTRNO        2   +  1
098100000613
098200111031     Otitv2i3ps E            testdett          2
098300000613     O                                              'Upload via Internet'
098400000613     O                                           +1 'Traduzione TIVIN00R -'
098500021113     O                                           +1 'FIVABWWR/FIVATWWR'
098600010201     O                                           +1 'Report testate bolle'
098700000616     O          E            testdett    3
098800000613     O                                           +2 'N° rec'
098900000613     O                                           +3 'Anteprima contenuto'
099000000616     O          E            rigadett    2
099100000613     O                       rrnum               +2
099200000621     O                       recko               +3
099300000616     O          E            findett     2
099400000613     O                       wrkdata
099500000613     O                       wrkora              +1
099600000613     O                       procname            +1
099700000616     O          E            findett     2
099800000613     O                                              'Cliente..................:'
099900000613     O                       VABCCM        z     +1
100000000616     O          E            findett     2
100100000613     O                                              'Riferimento Strategi.....:'
100200000613     O                       vlrhdl              +1
100300000616     O          E            findett     2
100400000613     O                                              'Giusti...................:'
100500010201     O                       §CTROKVB      2   +  1
100600100525     O                       §CTROKVT      2   +  1
100700000616     O          E            findett     2
100800000613     O                                              'Sbagliati e corretti.....:'
100900000613     O                       §CTRMO        2   +  1
101000000616     O          E            findett     2
101100000613     O                                              'Sbagliati e scartati.....:'
101200000613     O                       §CTRNO        2   +  1
101300000512** CMD - COMANDI CL
101400111031OVRPRTF FILE(TITV2I3P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
101500111031OVRPRTF FILE(TITV2I3PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
101600111031DLTOVR FILE(TITV2I3P)  LVL(*)
101700111031DLTOVR FILE(TITV2I3PS) LVL(*)
101800000512
