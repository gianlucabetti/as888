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
026000101119     C                   exsr      inzvar
026100101119     C                   exsr      defcam
026200071003     C                   exsr      impvab
026300100525     C                   exsr      wrivab
026400111031     C                   exsr      exevata                                      => carico VAT
026500111031     C                   exsr      exevatb                                      => carico VAT
026600111031     C   51              exsr      exevate                                      => carico VAT
026700071003     C                   else
026800080923     C*
026900071009     C                   if        wDISK = 'D'
027000091223     C                   exsr      repNSP
027100071009     C                   exsr      impvab
027200100412     C                   exsr      wrivab
027300111031     C                   exsr      exevata                                      => carico VAT
027400111031     C                   exsr      exevatb                                      => carico VAT
027500111031     C                   exsr      exevate                                      => carico VAT
027600071009     C                   else
027700071009     C*
027800010305     C                   if        depspe = *blanks                             => 1° giro
027900010305     C                   eval      depspe = curspe                              => memorizz. spediz
028000080117     C                   clear                   tivinds
028100091223     C                   exsr      repNSP
028200020305     C                   exsr      impvab
028300111031     C                   exsr      exevata                                      => carico VAT
028400111031     C                   exsr      exevatb                                      => carico VAT
028500111031     C   50              exsr      exevate                                      => carico VAT
028600010305     C                   else
028700020725     C                   if        depspe <> curspe                             => rottura di spediz
028800010305     C                   eval      depspe = curspe                              => memorizz. spediz
028900100412     C                   exsr      wrivab
029000080117     C                   clear                   tivinds
029100091223     C                   exsr      repNSP
029200020305     C                   exsr      impvab
029300111031     C                   exsr      exevata                                      => carico VAT
029400111031     C                   exsr      exevatb                                      => carico VAT
029500111031     C   50              exsr      exevate                                      => carico VAT
029600020305     C                   else                                                   => x stessa spediz
029700100401     C*
029800100401     C* Se nn richiesta esclusione spedizioni "duplicate"
029900100928     C                   select
030000100928     C                   when      wDUPREC = ' '
030100020305     C                   exsr      impvab
030200111031     C                   exsr      exevata                                      => carico VAT
030300111031     C                   exsr      exevatb                                      => carico VAT
030400111031     C   50              exsr      exevate                                      => carico VAT
030500100928     C                   when      wDUPREC = 'C'
030600111031     C   50              exsr      exevate                                      => carico VAT
030700100928     C                   endsl
030800010305     C                   endif
030900010305     C                   endif
031000010305     C                   endif
031100071003     C                   endif
031200110201     C*
031300071009     C                   endif
031400000905     C*
031500000905     C                   else
031600020725     C                   eval      x_vinflg = '1'
031700000905     C                   endif
031800000905     C*
031900020725     C     VINRRN        chain     tivin000
032000020725     C                   if        %found(tivin00r)
032100020725     C                   eval      y_vinflg = x_vinflg
032200020725     C                   eval      y_vinmsg = x_vinmsg
032300020725     C                   update    tivin000
032400020725     C                   endif
032500020725     C*
032600020725     C/EXEC SQL
032700070530     C+ Fetch C1 into :INPUT_DS
032800020725     C/END-EXEC
032900020725     C*
033000020725     C                   enddo
033100020725     C*
033200020725     C/EXEC SQL
033300020725     C+ close C1
033400020725     C/END-EXEC
033500000905     C*
033600020305     C* Scarico i VAB rimasti "in sospeso"
033700071009     C                   if        wDISK = 'C'
033800100412     C                   exsr      wrivab
033900071009     C                   endif
034000010202     C*
034100010202     C                   endif
034200990910
034300990910     C* Se non ci sono record con errori ...
034400000710     C                   if        §ctrno = 0
034500990910     C* ... restituisco esito OK.
034600990921     C                   eval      wrkesito = '0'
034700990910     C                   else
034800100525     C                   if        §ctrokvb > 0 OR
034900100525     C                             §ctrokvt > 0
035000990921     C                   eval      wrkesito = '1'
035100000710     C                   else
035200000710     C                   eval      wrkesito = '2'
035300990910     C                   endif
035400000710     C                   endif
035500990910     C*
035600990914     C                   if        %open(tivin00r)
035700990908     C                   close     tivin00r
035800990914     C                   endif
035900021113     C                   if        %open(fivabwwr)
036000021113     C                   close     fivabwwr
036100990914     C                   endif
036200021113     C                   if        %open(fivatwwr)
036300021113     C                   close     fivatwwr
036400010201     C                   endif
036500990910     C*
036600100525     C                   if        §ctrokvb > 0 OR
036700100525     C                             §ctrokvt > 0
036800000724     C                             and vlrpoi <> *zeros
036900010202     C                   exsr      invio
037000990920     C                   endif
037100990920     C*
037200910830     C                   ENDSR
037300000613     C***
037400100412
037500100412
037600010305
037700010305     C*----------------------------------------------------*
037800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
037900010305     C*----------------------------------------------------*
038000020305     C     WRIVAB        BEGSR
038100100412     C*
038200100412     C* Gestisco l'eventuale rottura x numero spedizione
038300100412     C                   if        VABRMA = *blanks
038400100412     C                   movel(P)  VABRMN        VABRMA
038500100412     C                   endif
038600100412     C*
038700100525     C                   if        *in51 = *off and *in31 = *off
038800100525     C                   write     fivab000                                     => scarico il VAB
038900100525     C                   endif
039000010305     C*
039100010305     C                   ENDSR
039200990920
039300000801     C*----------------------------------------------------*
039400000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
039500000801     C*----------------------------------------------------*
039600010201     C     INZVAR        BEGSR
039700000801     C*
039800010201     C                   Z-ADD     *zeros        Num5_0
039900020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
040000020725     C                   MOVEL     '0'           FlgCAS            1
040100000801     C*
040200000801     C                   ENDSR
040300000801     C*----------------------------------------------------*
040400000801     C*  IMPOSTAZIONE CAMPI COSTANTI
040500000801     C*----------------------------------------------------*
040600020904     C     DEFCAM        BEGSR
040700080923     C*
040800100928     C                   SETOFF                                       505160
040900000801     C*
041000021113     C                   CLEAR                   FIVAB000
041100021113     C                   CLEAR                   FIVAT000
041200070730     C* Imposto i valori di default...
041300111031     C                   EVAL      VABCCM = 0434461
041400111031     C                   EVAL      VATCCM = 0434461
041500111031     C                   EVAL      VABLNP = 043
041600111031     C                   EVAL      VATLNP = 043
041700111031     C                   EVAL      VABCTR = 000
041800100525     C                   EVAL      VABCBO = '1'
041900070222     C* ... e poi verifico se sono stati passati come parametri
042000070222     C                   IF        vlrppt > *blanks
042100100525     C*
042200070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
042300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
042400070222     C                   EXSR      CHKNUM
042500070222     C                   IF        PiInt=*on
042600070222     C                   Z-ADD     PiVal         VABCCM
042700070222     C                   Z-ADD     PiVal         VATCCM
042800070222     C                   ENDIF
042900070222     C                   ENDIF
043000070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
043100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
043200070222     C                   EXSR      CHKNUM
043300070222     C                   IF        PiInt=*on
043400070222     C                   Z-ADD     PiVal         VABLNP
043500070222     C                   Z-ADD     PiVal         VATLNP
043600070222     C                   ENDIF
043700070222     C                   ENDIF
043800070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
043900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
044000070222     C                   EXSR      CHKNUM
044100070222     C                   IF        PiInt=*on
044200070222     C                   Z-ADD     PiVal         VABCTR
044300070222     C                   ENDIF
044400070928     C                   ENDIF
044500071009     C                   MOVEL     *blanks       wDISK             1
044600071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
044700071009     C                   IF        wDISK <> *blanks
044800070928     C                   SETON                                        50
044900070222     C                   ENDIF
045000100525     C                   IF        wDISK  = 'T'
045100100525     C                   SETOFF                                       50
045200100525     C                   SETON                                        51
045300100525     C                   ENDIF
045400100401     C                   MOVEL     *blanks       wDUPREC           1
045500100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
045600100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
045700100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
045800100525     C                   ENDIF
045900111031     C                   MOVEL     *blanks       wNCL              1            1/A/*blanks
046000111031     C                   EVAL      wNCL = %subst(vlrppt:18:1)
046100100525     C*
046200070222     C                   ENDIF
046300071009     C*
046400071009     C   50              EVAL      VABCTM = '7Q'
046500000801     C*
046600000801     C                   ENDSR
046700091223     C*----------------------------------------------------*
046800091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
046900091223     C*----------------------------------------------------*
047000091223     C     REPNSP        BEGSR
047100091223     C*
047200091223     C                   EXSR      INZVAR
047300091223     C                   EXSR      DEFCAM
047400100928     C*
047500100928     C                   SETON                                        60
047600091223     C*
047700091223     C* NSP => Stacco un numeratore da AZNUM
047800091223     C                   clear                   TRUL33DS
047900091223     C                   eval      I33OPE = *zeros
048000091223     C                   eval      I33CNU = 302
048100091223     C                   eval      I33NUM = 1
048200091223     C                   movel     TRUL33DS      KPJBU
048300091223     C                   call      'TRUL33R'
048400091223     C                   parm                    KPJBA
048500091223     C                   movel     KPJBU         TRUL33DS
048600091223     C                   if        O33ERR = *zeros
048700091223     C                   z-add     O33NRF        VABNSP
048800091223     C                   z-add     O33NRF        VATNSP
048900091223     C                   else
049000091223     C                   SETON                                        31
049100091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049200091223     C                             + ' ' + 'VABNSP VATNSP'
049300091223     C                   endif
049400091223     C*
049500091223     C                   ENDSR
049600111031     C*----------------------------------------------------*
049700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
049800111031     C*----------------------------------------------------*
049900111031     C     IMPVAB        BEGSR
050000111031     C*
050100111031     C                   Z-ADD     *zeros        errore            1 0
050200111031     C                   MOVEL     datcor        VABAAS
050300111031     C                   MOVEL     datcor        VATAAS
050400111031     C                   MOVE      datcor        VABMGS
050500111031     C                   MOVE(P)   vlrpoi        VABFGS
050600111031     C                   MOVE(P)   vlrpoi        VATFGS
050700111031     C*
050800111031     C                   EVAL      VABRSD=%trim(%subst(vindta:276:40))
050900111031     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
051000111031     C     '@':'A'       XLATE     VABRSD        VABRSD
051100111031     C* ==
051200111031     C                   EVAL      VABRD2=%trim(%subst(vindta:316:20))
051300111031     C                   EVAL      VABIND=%trim(%trim(%subst(vindta:336:5))+' '+
051400111031     C                                    %trim(%subst(vindta:341:30))+' '+
051500111031     C                                    %trim(%subst(vindta:371:5)))
051600111031     C                   EVAL      VABLOD=%trim(%subst(vindta:400:30))
051700111031     C                   EVAL      VABPRD=%trim(%subst(vindta:430:2))
051800111031     C***                EVAL      VABRMA=%trim(%subst(vindta:46:10))
051900111031     C                   EVAL      VABRMA=%trim(%subst(vindta:46:25))
052000111031     C                   EVAL      VABNOT=%trim(%subst(vindta:514:30))
052100111031     C                   IF        %subst(vindta:43:1) = 'N'
052200111031     C                   EVAL      VABCBO='1'
052300111031     C                   ENDIF
052400111031     C                   IF        %subst(vindta:43:1) = 'P'
052500111031     C                   EVAL      VABCBO='2'
052600111031     C                   ENDIF
052700111031     C                   IF        %subst(vindta:511:3) = 'ABM'
052800111031     C                   EVAL      VABTIC='BM'
052900111031     C                   ENDIF
053000111031     C                   IF        %subst(vindta:511:3) = 'ABS'
053100111031     C                   EVAL      VABTIC='B'
053200111031     C                   ENDIF
053300111031     C                   IF        %subst(vindta:511:3) = 'CON'
053400111031     C                   EVAL      VABTIC=*blanks
053500111031     C                   ENDIF
053600111031     C                   IF        %subst(vindta:511:3) = 'ACM'
053700111031     C                   EVAL      VABTIC='CM'
053800111031     C                   ENDIF
053900111031     C                   IF        %subst(vindta:511:3) = 'ACS'
054000111031     C                   EVAL      VABTIC='CB'
054100111031     C                   ENDIF
054200111031     C                   IF        %subst(vindta:511:3) = 'VAR'
054300111031     C                   EVAL      VABTIC=*blanks
054400111031     C                   ENDIF
054500111031     C* CAP
054600111031     C                   EVAL      PiStr=%trim(%subst(vindta:391:5))
054700111031     C                   EXSR      CHKNUM
054800111031     C                   IF        PiInt=*on
054900111031     C                   Z-ADD     PiVal         Num5_0
055000111031     C                   MOVEL(P)  Num5_0        VABCAD
055100111031     C                   ELSE
055200111031     C                   ADD       1             errore
055300111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055400111031     C                             + ' ' + 'VABCAD'
055500111031     C                   ENDIF
055600111031     C* Reperisco la provincia dal CAP e dalla località
055700111031     C                   IF        VABCAD <> *blanks AND
055800111031     C                             VABPRD  = *blanks
055900111031     C                   CLEAR                   TISI95DS
056000111031     C                   EVAL      I95TCN = '3'
056100111031     C                   Z-ADD     datcor        I95DAT
056200111031     C                   EVAL      I95CAP = VABCAD
056300111031     C                   EVAL      I95LOC = VABLOD
056400111031     C                   EVAL      I95NAR = VABNZD
056500111031     C                   CALL      'TISI95R'
056600111031     C                   PARM                    TISI95DS
056700111031     C                   EVAL      VABPRD = O95PRV
056800111031     C                   ENDIF
056900111031     C* NCL
057000111031     C                   EVAL      PiStr=%trim(%subst(vindta:432:3))
057100111031     C                   EXSR      CHKNUM
057200111031     C                   IF        PiInt=*on
057300111031     C                   SELECT
057400111031     C                   WHEN      wNCL = *blanks
057500111031     C                   Z-ADD     PiVal         VABNCL
057600111031     C                   WHEN      wNCL = 'A'
057700111031     C                   ADD       PiVal         VABNCL
057800111031     C                   WHEN      wNCL = '1'
057900111031     C                   ADD       1             VABNCL
058000111031     C                   ENDSL
058100111031     C                   ELSE
058200111031     C                   ADD       1             errore
058300111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058400111031     C                             + ' ' + 'VABNCL'
058500111031     C                   ENDIF
058600111031     C* PKB
058700111031     C                   EVAL      PiStr=%trim(%subst(vindta:435:7))
058800111031     C                   EXSR      CHKNUM
058900111031     C                   IF        PiNum=*on
059000111031     C                   EVAL      PiVal=PiVal/1000                             da grammi a kg
059100111031     C                   Z-ADD     PiVal         VABPKB
059200111031     C                   ELSE
059300111031     C                   ADD       1             errore
059400111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
059500111031     C                             + ' ' + 'VABPKB'
059600111031     C                   ENDIF
059700111031     C* RMN
059800111031     C***                EVAL      PiStr=%trim(%subst(vindta:46:25))
059900111031     C                   EVAL      PiStr=%trim(%subst(vindta:46+10:15))
060000111031     C***                EVAL      PiStr=%trim(%subst(vindta:13:30))
060100111031     C***                EVAL      PiStr=%trim(%subst(vindta:47:24))
060200111031     C                   EXSR      CHKNUM
060300111031     C                   IF        PiInt=*on
060400111031     C                   Z-ADD     PiVal         VABRMN
060500111031     C                   ELSE
060600111031     C                   ADD       1             errore
060700111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
060800111031     C                             + ' ' + 'VABRMN'
060900111031     C                   ENDIF
061000111031     C* CAS
061100111031     C                   IF        %trim(%subst(vindta:502:9))<>'000000.00' AND
061200111031     C                             %trim(%subst(vindta:502:9))<>*blanks     AND
061300111031     C                             %trim(%subst(vindta:502:9))<>*zeros
061400111031     C                   EVAL      VABVCA = 'EUR'
061500120315     C                   EVAL      VABTIC = 'TM'
061600111031     C                   EVAL      FlgCAS = '1'
061700111031     C                   EVAL      PiStr=%trim(%subst(vindta:502:9))
061800111031     C                   EXSR      CHKNUM
061900111031     C                   IF        PiNum=*on
062000111031     C                   Z-ADD     PiVal         VABCAS
062100111031     C                   ELSE
062200111031     C                   ADD       1             errore
062300111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
062400111031     C                             + ' ' + 'VABCAS'
062500111031     C                   ENDIF
062600111031     C                   ENDIF
062700111031     C* IAS
062800111031     C                   IF        %trim(%subst(vindta:544:10)) <> *blanks
062900111031     C                   EVAL      VABVAS = 'EUR'
063000111031     C                   EVAL      PiStr=%trim(%subst(vindta:544:10))
063100111031     C                   EXSR      CHKNUM
063200111031     C                   IF        PiNum=*on
063300111031     C                   Z-ADD     PiVal         VABIAS
063400111031     C                   ELSE
063500111031     C                   ADD       1             errore
063600111031     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
063700111031     C                             + ' ' + 'VABIAS'
063800111031     C                   ENDIF
063900111031     C                   ENDIF
064000111031     C*
064100111031     C* Considerazioni sul contenuto di campi precedentemente valorizzati
064200111031     C                   IF        FlgCAS <> '0'
064300111031     C                   IF        VABCBO = '1'
064400111031     C                   EVAL      VABCBO = '4'
064500111031     C                   ELSE
064600111031     C                   EVAL      VABCBO = '6'
064700111031     C                   ENDIF
064800111031     C                   ENDIF
064900111031     C*
065000111031     C* Eseguo routine finale x considerazioni specifiche su importi/divise
065100111031     C                   EXSR      CHKIMPDIV
065200111031     C*
065300111031     C* Ebbene...
065400111031     C                   ADD       1             §CTRMO
065500111031     C                   IF        errore <> *zeros
065600111031     C                   ADD       1             §CTRNO
065700111031     C                   EVAL      recko = vindta
065800111031     C                   EXCEPT    rigadett
065900111031     C                   EVAL      vinflg = '2'
066000111031     C                   ELSE
066100111031     C                   ADD       1             §CTROKVB
066200111031     C                   ENDIF
066300111031     C*
066400111031     C                   ENDSR
066500111031
066600111031
066700111031     C*----------------------------------------------------*
066800111031     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
066900111031     C*----------------------------------------------------*
067000111031     C     CHKIMPDIV     BEGSR
067100111031     C*
067200111031     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
067300111031     C                   Z-ADD     *zeros        wrkDec            9 9
067400111031     C*
067500111031     C* Come prima cosa effettuo considerazioni sulla divisa
067600111031     C                   IF        vabIAS > *zeros
067700111031     C                   IF        vabVAS <> 'EUR'
067800111031     C                   EVAL      vabVAS =  'ITL'
067900111031     C                   ENDIF
068000111031     C                   ENDIF
068100111031     C*
068200111031     C                   IF        vabCAS > *zeros
068300111031     C                   IF        vabVCA <> 'EUR'
068400111031     C                   EVAL      vabVCA =  'ITL'
068500111031     C                   ENDIF
068600111031     C                   ENDIF
068700111031     C*
068800111031     C                   IF        vabVMD > *zeros
068900111031     C                   IF        vabVAD <> 'EUR'
069000111031     C                   EVAL      vabVAD =  'ITL'
069100111031     C                   ENDIF
069200111031     C                   ENDIF
069300111031     C*
069400111031     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
069500111031     C                   Z-ADD     vabIAS        wrkDec
069600111031     C                   IF        wrkDec > *zeros
069700111031     C                   IF        vabVAS = 'ITL'
069800111031     C                   EVAL      vabIAS = *zeros
069900111031     C                   ENDIF
070000111031     C                   ENDIF
070100111031     C*
070200111031     C* Stabilisco se il contrasegno ha decimali valorizzati
070300111031     C                   Z-ADD     vabCAS        wrkDec
070400111031     C                   IF        wrkDec > *zeros
070500111031     C                   IF        vabVCA = 'ITL'
070600111031     C                   EVAL      vabCAS = *zeros
070700111031     C                   ENDIF
070800111031     C                   ENDIF
070900111031     C*
071000111031     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
071100111031     C                   Z-ADD     vabVMD        wrkDec
071200111031     C                   IF        wrkDec > *zeros
071300111031     C                   IF        vabVAD = 'ITL'
071400111031     C                   EVAL      vabVMD = *zeros
071500111031     C                   ENDIF
071600111031     C                   ENDIF
071700111031     C*
071800111031     C                   ENDSR
071900111031     C***
072000111031
072100111031
072200111031     C*----------------------------------------------------*
072300111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "A"
072400111031     C*----------------------------------------------------*
072500111031     C     EXEVATA       BEGSR
072600111031     C*
072700111031     C***                EVAL      VATTRC='A'
072800111031     C***                EVAL      VATNOT = %trim(%subst(vindta:581:30))
072900111031     C***                exsr      wrivat                                       => scarico VAT
073000111031     C*
073100111031     C                   ENDSR
073200111031     C*----------------------------------------------------*
073300111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "B"
073400111031     C*----------------------------------------------------*
073500111031     C     EXEVATB       BEGSR
073600111031     C*
073700111031     C***                EVAL      VATTRC='B'
073800111031     C***                EVAL      VATNOT = %trim(%subst(vindta:376:15))
073900111031     C***                exsr      wrivat                                       => scarico VAT
074000111031     C*
074100111031     C                   ENDSR
074200111031     C*----------------------------------------------------*
074300111031     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT) - TIPO RECORD "E"
074400111031     C*----------------------------------------------------*
074500111031     C     EXEVATE       BEGSR
074600111031     C*
074700111031     C                   EVAL      VATTRC='E'
074800111031     C                   EVAL      VATNOT = %trim(%subst(vindta:554:25))
074900111031     C                   exsr      wrivat                                       => scarico VAT
075000111031     C*
075100111031     C                   ENDSR
075200111031     C*----------------------------------------------------*
075300111031     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
075400111031     C*----------------------------------------------------*
075500111031     C     WRIVAT        BEGSR
075600111031     C*
075700111031     C* Scrivo solo se valorizzato qualcosa
075800111031     C                   IF        VATNOT <> *blanks
075900111031     C                   WRITE     FIVAT000
076000111031     C                   ENDIF
076100111031     C*
076200111031     C                   ENDSR
076300020904
076400020904
076500010202     C*----------------------------------------------------*
076600100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
076700010202     C*----------------------------------------------------*
076800100525     C     PREVASX       BEGSR
076900100525     C*
077000021113     C* Compongo il nome del membro da dare al FIVATWWR
077100010202     C                   eval      parmbr = vlrhdl
077200010202     C                   movel     'M'           parmbr
077300070530     C                   eval      parccm = %subst(vlrKSC:2:7)
077400010202     C                   eval      paropz = '1'
077500100525     C*
077600010202     C* Effettuo la chiamata al CLLE preposto
077700100525     C  N51              call(e)   'TITVVTC'
077800010202     C                   parm                    parccm
077900010202     C                   parm                    parmbr
078000010202     C                   parm                    paropz
078100100525     C   51              call(e)   'TITVVBC'
078200100525     C                   parm                    parccm
078300100525     C                   parm                    parmbr
078400100525     C                   parm                    paropz
078500100525     C*
078600010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
078700010202     C                   if        %error
078800010202     C                   movel     '1'           chkcall
078900010202     C                   else
079000010202     C                   movel     '0'           chkcall
079100010202     C                   endif
079200010202     C*
079300010202     C                   ENDSR
079400000801     C*----------------------------------------------------*
079500000801     C*  CONTROLLO NUMERICITA' CAMPI
079600000801     C*----------------------------------------------------*
079700000801     C     CHKNUM        BEGSR
079800081113     C*
079900081113     C                   IF        PiDecChr = *blanks
080000110408     C                   EVAL      PiDecChr = '.'
080100081113     C                   ENDIF
080200091223     C*
080300081113     C                   callp(e)  UBISNUM_Check(PiStr
080400081113     C                                          :PiDecChr
080500081113     C                                          :PiVal
080600081113     C                                          :PiNum
080700081113     C                                          :PiInt)
080800081113     C*
080900000801     C                   IF        %error
081000000801     C                   EVAL      PiInt=*off
081100000801     C                   ENDIF
081200000801     C*
081300000801     C                   ENDSR
081400000801     C***
081500011113
081600011113
081700000801
081800000801
081900990920      /TITLE Invio dei dati al punto operativo.
082000010202     C     invio         BEGSR
082100990920     C*
082200021113     C* 1° invio FIVAT
082300010201     C                   reset                   dscmz
082400110411     C                   move      vatfgs        cmzdst
082500021113     C                   eval      cmzfld = 'FIVATWWR'
082600010201     C                   eval      cmzmbd = vlrhdl
082700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
082800021009     C***                if        prmfir = *blanks
082900021113     C                   eval      cmzfla = 'FIVAT00F'
083000021113     C                   eval      cmzmba = 'FIVAT00F'
083100021009     C***                else
083200021009     C***                eval      cmzfla = prmfir
083300021009     C***                eval      cmzmba = prmfir
083400021009     C***                endif
083500010201     C                   eval      cmznrr = *zeros
083600020305     C                   move      §ctrokvt      cmznrr
083700021018     C                   eval      cmzlba = vlrfl1
083800010201     C                   call(e)   'TIS711C'
083900010201     C                   parm                    dscmz
084000010201     C                   parm      *blanks       esito
084100010205     C                   if        %error
084200010205     C                             or cmzerr = '1'
084300010205     C                             or esito  = '1'
084400010205     C                   eval      wrkesito = '3'
084500010205     C                   else
084600010201     C*
084700021113     C* 2° invio FIVAB
084800100525     C                   if        *in51 = *off
084900010201     C                   reset                   dscmz
085000110411     C                   move      vabfgs        cmzdst
085100010201     C                   eval      cmzfld = vlrfou
085200010201     C                   eval      cmzmbd = vlrhdl
085300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
085400021009     C***                if        prmfir = *blanks
085500021113     C                   eval      cmzfla = 'FIVAB00F'
085600021113     C                   eval      cmzmba = 'FIVAB00F'
085700021009     C***                else
085800021009     C***                eval      cmzfla = prmfir
085900021009     C***                eval      cmzmba = prmfir
086000021009     C***                endif
086100010201     C                   eval      cmznrr = *zeros
086200010201     C                   move      §ctrokvb      cmznrr
086300021018     C                   eval      cmzlba = vlrfl1
086400010201     C                   call(e)   'TIS711C'
086500010201     C                   parm                    dscmz
086600010201     C                   parm      *blanks       esito
086700010201     C                   if        %error
086800010201     C                             or cmzerr = '1'
086900010201     C                             or esito  = '1'
087000010201     C                   eval      wrkesito = '3'
087100010201     C                   endif
087200100525     C                   endif
087300010205     C                   endif
087400990920     C*
087500000613     C                   ENDSR
087600000613     C***
087700070411
087800110627     C     *pssr         BEGSR
087900070411     C*
088000110627     C                   if        %open(tivin00r)
088100110627     C                   close     tivin00r
088200110627     C                   endif
088300110627     C                   if        %open(fivabwwr)
088400110627     C                   close     fivabwwr
088500110627     C                   endif
088600110627     C                   if        %open(fivatwwr)
088700110627     C                   close     fivatwwr
088800110627     C                   endif
088900110627     C*
089000110627     C* Effettuo la chiamata al CLLE preposto
089100110627     C  N51              call(e)   'TITVVTC'
089200110627     C                   parm                    parccm
089300110627     C                   parm                    parmbr
089400110627     C                   parm      '2'           paropz
089500110627     C   51              call(e)   'TITVVBC'
089600110627     C                   parm                    parccm
089700110627     C                   parm                    parmbr
089800110627     C                   parm      '2'           paropz
089900110627     C*
090000110627     C                   eval      wrkesito = '2'
090100110627     C*
090200110627     C                   seton                                        LR
090300110627     C*
090400110627     C                   ENDSR     '*CANCL'
090500070411     C***
090600070411
090700990910
090800000613     C     *inzsr        BEGSR
090900990910     C*
091000990910     C     *entry        plist
091100990920     C                   parm                    tivlrds
091200990921     C                   parm      wrkesito      esito
091300000724     C                   parm                    prmlit
091400000710     C                   parm                    prmfir
091500000613     C*
091600000830     C* CALCOLA LA DATA CORRENTE
091700091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
091800091223     C                   eval      datcor = %dec(%date() : *ISO)
091900000830     C*
092000000613     C                   ENDSR
092100000613     C***
092200990908
092300111031     Otitv2i3p  E            riepilogo         2
092400990915     O                                              'Upload via Internet'
092500990915     O                                           +1 'Traduzione TIVIN00R -'
092600021113     O                                           +1 'FIVABWWR/FIVATWWR'
092700010201     O                                           +1 'Report testate bolle'
092800990915     O          E            riepilogo   2
092900990915     O                       wrkdata
093000990915     O                       wrkora              +1
093100990915     O                       procname            +1
093200990915     O          E            riepilogo   2
093300990915     O                                              'Cliente..................:'
093400990915     O                       VABCCM        z     +1
093500990915     O          E            riepilogo   2
093600990920     O                                              'Riferimento Strategi.....:'
093700990920     O                       vlrhdl              +1
093800990915     O          E            riepilogo   2
093900990915     O                                              'Giusti...................:'
094000010201     O                       §CTROKVB      2   +  1
094100100525     O                       §CTROKVT      2   +  1
094200990915     O          E            riepilogo   2
094300990915     O                                              'Sbagliati e corretti.....:'
094400971022     O                       §CTRMO        2   +  1
094500990915     O          E            riepilogo   2
094600990915     O                                              'Sbagliati e scartati.....:'
094700971022     O                       §CTRNO        2   +  1
094800000613
094900111031     Otitv2i3ps E            testdett          2
095000000613     O                                              'Upload via Internet'
095100000613     O                                           +1 'Traduzione TIVIN00R -'
095200021113     O                                           +1 'FIVABWWR/FIVATWWR'
095300010201     O                                           +1 'Report testate bolle'
095400000616     O          E            testdett    3
095500000613     O                                           +2 'N° rec'
095600000613     O                                           +3 'Anteprima contenuto'
095700000616     O          E            rigadett    2
095800000613     O                       rrnum               +2
095900000621     O                       recko               +3
096000000616     O          E            findett     2
096100000613     O                       wrkdata
096200000613     O                       wrkora              +1
096300000613     O                       procname            +1
096400000616     O          E            findett     2
096500000613     O                                              'Cliente..................:'
096600000613     O                       VABCCM        z     +1
096700000616     O          E            findett     2
096800000613     O                                              'Riferimento Strategi.....:'
096900000613     O                       vlrhdl              +1
097000000616     O          E            findett     2
097100000613     O                                              'Giusti...................:'
097200010201     O                       §CTROKVB      2   +  1
097300100525     O                       §CTROKVT      2   +  1
097400000616     O          E            findett     2
097500000613     O                                              'Sbagliati e corretti.....:'
097600000613     O                       §CTRMO        2   +  1
097700000616     O          E            findett     2
097800000613     O                                              'Sbagliati e scartati.....:'
097900000613     O                       §CTRNO        2   +  1
098000000512** CMD - COMANDI CL
098100111031OVRPRTF FILE(TITV2I3P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
098200111031OVRPRTF FILE(TITV2I3PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
098300111031DLTOVR FILE(TITV2I3P)  LVL(*)
098400111031DLTOVR FILE(TITV2I3PS) LVL(*)
098500000512
