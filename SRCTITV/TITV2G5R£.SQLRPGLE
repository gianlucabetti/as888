000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600100412     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800110627     Ftitv2g5p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000110627     F                                     infsr(*pssr)
001100110627     Ftitv2g5ps O    f  198        PRINTER usropn
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
003600110627     D depspe          s             11    INZ(*blanks)
003700110627     D curspe          s             11    INZ(*blanks)
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
005100110627     D  VINSPE                       11
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
012400110627     C                   if        not %open(titv2g5ps)
012500110627     C                   open      titv2g5ps
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
015100110627     C                   if        %open(titv2g5ps)
015200000616     C                   except    findett
015300110627     C                   close     titv2g5ps
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
018000110627     C                   if        not %open(titv2g5p)
018100110627     C                   open      titv2g5p
018200990915     C                   endif
018300990915     C*
018400110411     C                   except    riepilogo
018500990915     C*
018600110627     C                   if        %open(titv2g5p)
018700110627     C                   close     titv2g5p
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
022600110627     C+ vindta, vinflg, substr(vindta, 217, 11) as sped, rrn(tivin00r)
022700060223     C+ from tivin00r
022800110627     C+ order by sped, substr(vindta, 217, 11) desc
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
025600110627     C                   EVAL      curspe=%trim(%subst(vindta:217:11))
025700010305     C*
025800100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
025900080923     C                   if        *in50 = *off
026000101119     C                   exsr      inzvar
026100101119     C                   exsr      defcam
026200071003     C                   exsr      impvab
026300100525     C                   exsr      wrivab
026400100127     C                   exsr      wrivat_a                                     => carico VAT
026500071003     C                   exsr      wrivat_b                                     => carico VAT
026600100525     C   51              exsr      wrivat_e                                     => carico VAT
026700071003     C                   else
026800080923     C*
026900071009     C                   if        wDISK = 'D'
027000091223     C                   exsr      repNSP
027100071009     C                   exsr      impvab
027200100412     C                   exsr      wrivab
027300100127     C                   exsr      wrivat_a                                     => carico VAT
027400071009     C                   exsr      wrivat_b                                     => carico VAT
027500071009     C                   exsr      wrivat_e                                     => carico VAT
027600071009     C                   else
027700071009     C*
027800010305     C                   if        depspe = *blanks                             => 1° giro
027900010305     C                   eval      depspe = curspe                              => memorizz. spediz
028000080117     C                   clear                   tivinds
028100091223     C                   exsr      repNSP
028200020305     C                   exsr      impvab
028300100127     C                   exsr      wrivat_a                                     => carico VAT
028400071003     C                   exsr      wrivat_b                                     => carico VAT
028500071003     C   50              exsr      wrivat_e                                     => carico VAT
028600010305     C                   else
028700020725     C                   if        depspe <> curspe                             => rottura di spediz
028800010305     C                   eval      depspe = curspe                              => memorizz. spediz
028900100412     C                   exsr      wrivab
029000080117     C                   clear                   tivinds
029100091223     C                   exsr      repNSP
029200020305     C                   exsr      impvab
029300100127     C                   exsr      wrivat_a                                     => carico VAT
029400071003     C                   exsr      wrivat_b                                     => carico VAT
029500071003     C   50              exsr      wrivat_e                                     => carico VAT
029600020305     C                   else                                                   => x stessa spediz
029700100401     C*
029800100401     C* Se nn richiesta esclusione spedizioni "duplicate"
029900100928     C                   select
030000100928     C                   when      wDUPREC = ' '
030100020305     C                   exsr      impvab
030200100127     C                   exsr      wrivat_a                                     => carico VAT
030300071003     C                   exsr      wrivat_b                                     => carico VAT
030400071003     C   50              exsr      wrivat_e                                     => carico VAT
030500100928     C                   when      wDUPREC = 'C'
030600100928     C   50              exsr      wrivat_e                                     => carico VAT
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
038600110627     C*
038700110627     C* Gestisco considerazioni x codice tariffa
038800110627     C                   if        FlgCTR = '1'
038900110627     C                   eval      VABCTR = 100
039000110627     C                   endif
039100100412     C*
039200100525     C                   if        *in51 = *off and *in31 = *off
039300100525     C                   write     fivab000                                     => scarico il VAB
039400100525     C                   endif
039500010305     C*
039600010305     C                   ENDSR
039700990920
039800000801     C*----------------------------------------------------*
039900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
040000000801     C*----------------------------------------------------*
040100010201     C     INZVAR        BEGSR
040200000801     C*
040300010201     C                   Z-ADD     *zeros        Num5_0
040400020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
040500020725     C                   MOVEL     '0'           FlgCAS            1
040600110627     C* Inizializzo flag d wrk x codice tariffa
040700110627     C                   MOVEL     '1'           FlgCTR            1
040800000801     C*
040900000801     C                   ENDSR
041000000801     C*----------------------------------------------------*
041100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
041200000801     C*----------------------------------------------------*
041300020904     C     DEFCAM        BEGSR
041400080923     C*
041500100928     C                   SETOFF                                       505160
041600000801     C*
041700021113     C                   CLEAR                   FIVAB000
041800021113     C                   CLEAR                   FIVAT000
041900070730     C* Imposto i valori di default...
042000110627     C                   EVAL      VABCCM = 0410544
042100110627     C                   EVAL      VATCCM = 0410544
042200110627     C                   EVAL      VABLNP = 041
042300110627     C                   EVAL      VATLNP = 041
042400110627     C                   EVAL      VABCTR = 000
042500100525     C                   EVAL      VABCBO = '1'
042600070222     C* ... e poi verifico se sono stati passati come parametri
042700070222     C                   IF        vlrppt > *blanks
042800100525     C*
042900070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
043000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
043100070222     C                   EXSR      CHKNUM
043200070222     C                   IF        PiInt=*on
043300070222     C                   Z-ADD     PiVal         VABCCM
043400070222     C                   Z-ADD     PiVal         VATCCM
043500070222     C                   ENDIF
043600070222     C                   ENDIF
043700070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
043800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
043900070222     C                   EXSR      CHKNUM
044000070222     C                   IF        PiInt=*on
044100070222     C                   Z-ADD     PiVal         VABLNP
044200070222     C                   Z-ADD     PiVal         VATLNP
044300070222     C                   ENDIF
044400070222     C                   ENDIF
044500070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
044600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
044700070222     C                   EXSR      CHKNUM
044800070222     C                   IF        PiInt=*on
044900070222     C                   Z-ADD     PiVal         VABCTR
045000070222     C                   ENDIF
045100070928     C                   ENDIF
045200071009     C                   MOVEL     *blanks       wDISK             1
045300071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
045400071009     C                   IF        wDISK <> *blanks
045500070928     C                   SETON                                        50
045600070222     C                   ENDIF
045700100525     C                   IF        wDISK  = 'T'
045800100525     C                   SETOFF                                       50
045900100525     C                   SETON                                        51
046000100525     C                   ENDIF
046100100401     C                   MOVEL     *blanks       wDUPREC           1
046200100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
046300100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
046400100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
046500100525     C                   ENDIF
046600100525     C*
046700070222     C                   ENDIF
046800071009     C*
046900071009     C   50              EVAL      VABCTM = '7Q'
047000000801     C*
047100000801     C                   ENDSR
047200091223     C*----------------------------------------------------*
047300091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
047400091223     C*----------------------------------------------------*
047500091223     C     REPNSP        BEGSR
047600091223     C*
047700091223     C                   EXSR      INZVAR
047800091223     C                   EXSR      DEFCAM
047900100928     C*
048000100928     C                   SETON                                        60
048100091223     C*
048200091223     C* NSP => Stacco un numeratore da AZNUM
048300091223     C                   clear                   TRUL33DS
048400091223     C                   eval      I33OPE = *zeros
048500091223     C                   eval      I33CNU = 302
048600091223     C                   eval      I33NUM = 1
048700091223     C                   movel     TRUL33DS      KPJBU
048800091223     C                   call      'TRUL33R'
048900091223     C                   parm                    KPJBA
049000091223     C                   movel     KPJBU         TRUL33DS
049100091223     C                   if        O33ERR = *zeros
049200091223     C                   z-add     O33NRF        VABNSP
049300091223     C                   z-add     O33NRF        VATNSP
049400091223     C                   else
049500091223     C                   SETON                                        31
049600091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049700091223     C                             + ' ' + 'VABNSP VATNSP'
049800091223     C                   endif
049900091223     C*
050000091223     C                   ENDSR
050100000801     C*----------------------------------------------------*
050200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
050300000801     C*----------------------------------------------------*
050400100730     C     IMPVAB        BEGSR
050500070530     C*
050600070530     C                   SETOFF                                       3132
050700070928     C*
050800070928     C                   MOVE(P)   vlrpoi        VABFGS
050900070928     C                   MOVE(P)   vlrpoi        VATFGS
051000070928     C*
051100070928     C                   MOVEL     datcor        VABAAS
051200070928     C                   MOVEL     datcor        VATAAS
051300070928     C                   MOVE      datcor        VABMGS
051400100729     C*
051500100729     C* Reperimento campi ALFA
051600110627     C                   EVAL      VABRSD=%trim(%subst(vindta:274:35))
051700110627     C                   EVAL      VABRMA=%trim(%subst(vindta:221:7))
051800110627     C                   EVAL      VABIND=%trim(%subst(vindta:309:35))
051900110627     C                   EVAL      VABLOD=%trim(%subst(vindta:354:35))
052000110627     C                   EVAL      VABPRD=%trim(%subst(vindta:393:2))
052100110627     C                   EVAL      VABCAD=%trim(%subst(vindta:344:10))
052200100729     C*
052300100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
052400100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
052500100729     C     '@':'A'       XLATE     VABRSD        VABRSD
052600100729     C* ==
052700100729     C*
052800100729     C* Reperimento campi NUMERICI
052900100729     C                   MOVEL     DATCOR        VABAAS
053000100729     C                   MOVE      DATCOR        VABMGS
053100110408     C*
053200110127     C* NSP / RMN
053300101119     C                   IF        *IN51 = *off
053400110627     C                   EVAL      PiStr=%subst(vindta:223:5)
053500100730     C                   EXSR      CHKNUM
053600100730     C                   IF        PiInt=*on
053700100730     C                   Z-ADD     PiVal         VABRMN
053800100928     C  N60              Z-ADD     PiVal         VABNSP
053900100928     C  N60              Z-ADD     PiVal         VATNSP
054000100730     C                   ELSE
054100100730     C                   SETON                                        32
054200100730     C                   Z-ADD     1             VABRMN
054300100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054400101119     C                             + ' ' + 'VABNSP VABRMN VATNSP'
054500100730     C                   ENDIF
054600101119     C*
054700101119     C                   ELSE
054800101119     C*
054900110627     C                   EVAL      PiStr=%subst(vindta:223:5)
055000101119     C                   EXSR      CHKNUM
055100101119     C                   IF        PiInt=*on
055200101119     C  N60              Z-ADD     PiVal         VATNSP
055300101119     C                   ELSE
055400101119     C                   SETON                                        32
055500101119     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055600101119     C                             + ' ' + 'VATNSP'
055700101119     C                   ENDIF
055800101119     C                   ENDIF
055900110627     C* NCL
056000110627     C                   EVAL      VABNCL = VABNCL + 1
056100100729     C* PKB
056200110627     C                   IF        %trim(%subst(vindta:454:8)) <> *all'0' AND
056300110627     C                             %trim(%subst(vindta:454:8)) <> *blanks
056400110627     C                   EVAL      PiStr=%trim(%subst(vindta:454:8))
056500100729     C                   EXSR      CHKNUM
056600100729     C                   IF        PiNum=*on
056700110627     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
056800110627     C                   ADD(H)    PiVal         VABPKB
056900100729     C                   ELSE
057000101119     C  N51              SETON                                        32
057100101119     C  N51              Z-ADD     *zeros        VABPKB
057200101119     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
057300100729     C                             + ' ' + 'VABPKB'
057400100729     C                   ENDIF
057500110325     C                   ENDIF
057600110627     C* VLB
057700110627     C                   IF        %trim(%subst(vindta:470:8)) <> *all'0' AND
057800110627     C                             %trim(%subst(vindta:470:8)) <> *blanks
057900110627     C                   EVAL      PiStr=%trim(%subst(vindta:470:8))
058000110627     C                   EXSR      CHKNUM
058100110627     C                   IF        PiNum=*on
058200110627     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
058300110627     C                   ADD(H)    PiVal         VABVLB
058400110627     C                   ELSE
058500110627     C  N51              SETON                                        32
058600110627     C  N51              Z-ADD     *zeros        VABVLB
058700110627     C  N51              EVAL      x_vinmsg = %trimr(x_vinmsg)
058800110627     C                             + ' ' + 'VABVLB'
058900110627     C                   ENDIF
059000110627     C                   ENDIF
059100110627     C* CTR
059200110627     C                   IF        %subst(vindta:426:3) <> 'BAU' AND
059300110627     C                             %subst(vindta:426:3) <> 'APP'
059400110627     C                   EVAL      FlgCTR = '0'
059500110627     C                   ENDIF
059600100729     C*
059700100729     C* Se provincia nn valorizzata la reperisco
059800100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
059900100729     C                   IF        VABNZD  = *blanks AND
060000100729     C                             VABPRD  = *blanks AND
060100100729     C                             VABCAD <> *blanks
060200100729     C                   CLEAR                   TISI95DS
060300100729     C                   EVAL      I95TCN = '3'
060400100729     C                   Z-ADD     datcor        I95DAT
060500100729     C                   EVAL      I95NAR = VABNZD
060600100729     C                   EVAL      I95CAP = VABCAD
060700100729     C                   EVAL      I95LOC = VABLOD
060800100729     C                   CALL      'TISI95R'
060900100729     C                   PARM                    TISI95DS
061000100729     C                   EVAL      VABPRD = O95PRV
061100100729     C                   ENDIF
061200100729     C*
061300100729     C* Considerazioni finali su CBO/CAS
061400100729     C                   IF        FlgCAS = '1'
061500100729     C                   IF        VABCBO = '1'
061600100729     C                   EVAL      VABCBO = '4'
061700100729     C                   ENDIF
061800100729     C                   IF        VABCBO = '2'
061900100729     C                   EVAL      VABCBO = '6'
062000100729     C                   ENDIF
062100100729     C                   ENDIF
062200010202     C*
062300000801     C* Ebbene...
062400000801     C                   ADD       1             §CTRMO
062500070530     C                   IF        *in31 <> *zeros OR
062600070530     C                             *in32 <> *zeros
062700000801     C                   ADD       1             §CTRNO
062800000801     C                   EVAL      recko = vindta
062900000801     C                   EXCEPT    rigadett
063000020725     C                   EVAL      x_vinflg = '2'
063100000801     C                   ELSE
063200010201     C                   ADD       1             §CTROKVB
063300000801     C                   ENDIF
063400000801     C*
063500000801     C                   ENDSR
063600100127     C*----------------------------------------------------*
063700100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
063800100127     C*----------------------------------------------------*
063900100127     C     WRIVAT_A      BEGSR
064000100127     C*
064100100127     C* Valorizzo l buffer di scrittura del FIVAT
064200100127     C* - TIPO RECORD "A"
064300101119     C***                eval      VATTRC = 'A'
064400101119     C***                eval      VATNOT = %trim(%subst(vindta:697:22))
064500101119     C***                exsr      wrivat
064600100127     C*
064700100127     C                   ENDSR
064800010201     C*----------------------------------------------------*
064900071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
065000010201     C*----------------------------------------------------*
065100071003     C     WRIVAT_B      BEGSR
065200010201     C*
065300021113     C* Valorizzo l buffer di scrittura del FIVAT
065400070928     C* - TIPO RECORD "B"
065500110627     C***                eval      VATTRC = 'B'
065600110627     C***                eval      VATNOT = %trim(%subst(vindta:206:15))
065700110627     C***                exsr      wrivat
065800010201     C*
065900010201     C                   ENDSR
066000071003     C*----------------------------------------------------*
066100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
066200071003     C*----------------------------------------------------*
066300071003     C     WRIVAT_E      BEGSR
066400071003     C*
066500071003     C* Valorizzo l buffer di scrittura del FIVAT
066600071003     C* - TIPO RECORD "E"
066700100729     C*
066800100928     C                   eval      VATTRC = 'E'
066900110627     C                   eval      VATNOT = %trim(%subst(vindta:415:11))
067000100928     C                   exsr      wrivat
067100071003     C*
067200071003     C                   ENDSR
067300100127     C*----------------------------------------------------*
067400100127     C*  SCARICO BUFFER FIVAT
067500100127     C*----------------------------------------------------*
067600100127     C     WRIVAT        BEGSR
067700100127     C*
067800100127     C                   if        vatNOT <> *blanks
067900100127     C                   ADD       1             §CTROKVT
068000100127     C                   write     FIVAT000
068100100127     C                   endif
068200100127     C*
068300100127     C                   ENDSR
068400020904
068500020904
068600010202     C*----------------------------------------------------*
068700100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
068800010202     C*----------------------------------------------------*
068900100525     C     PREVASX       BEGSR
069000100525     C*
069100021113     C* Compongo il nome del membro da dare al FIVATWWR
069200010202     C                   eval      parmbr = vlrhdl
069300010202     C                   movel     'M'           parmbr
069400070530     C                   eval      parccm = %subst(vlrKSC:2:7)
069500010202     C                   eval      paropz = '1'
069600100525     C*
069700010202     C* Effettuo la chiamata al CLLE preposto
069800100525     C  N51              call(e)   'TITVVTC'
069900010202     C                   parm                    parccm
070000010202     C                   parm                    parmbr
070100010202     C                   parm                    paropz
070200100525     C   51              call(e)   'TITVVBC'
070300100525     C                   parm                    parccm
070400100525     C                   parm                    parmbr
070500100525     C                   parm                    paropz
070600100525     C*
070700010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
070800010202     C                   if        %error
070900010202     C                   movel     '1'           chkcall
071000010202     C                   else
071100010202     C                   movel     '0'           chkcall
071200010202     C                   endif
071300010202     C*
071400010202     C                   ENDSR
071500000801     C*----------------------------------------------------*
071600000801     C*  CONTROLLO NUMERICITA' CAMPI
071700000801     C*----------------------------------------------------*
071800000801     C     CHKNUM        BEGSR
071900081113     C*
072000081113     C                   IF        PiDecChr = *blanks
072100110408     C                   EVAL      PiDecChr = '.'
072200081113     C                   ENDIF
072300091223     C*
072400081113     C                   callp(e)  UBISNUM_Check(PiStr
072500081113     C                                          :PiDecChr
072600081113     C                                          :PiVal
072700081113     C                                          :PiNum
072800081113     C                                          :PiInt)
072900081113     C*
073000000801     C                   IF        %error
073100000801     C                   EVAL      PiInt=*off
073200000801     C                   ENDIF
073300000801     C*
073400000801     C                   ENDSR
073500000801     C***
073600011113
073700011113
073800000801
073900000801
074000990920      /TITLE Invio dei dati al punto operativo.
074100010202     C     invio         BEGSR
074200990920     C*
074300021113     C* 1° invio FIVAT
074400010201     C                   reset                   dscmz
074500110411     C                   move      vatfgs        cmzdst
074600021113     C                   eval      cmzfld = 'FIVATWWR'
074700010201     C                   eval      cmzmbd = vlrhdl
074800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074900021009     C***                if        prmfir = *blanks
075000021113     C                   eval      cmzfla = 'FIVAT00F'
075100021113     C                   eval      cmzmba = 'FIVAT00F'
075200021009     C***                else
075300021009     C***                eval      cmzfla = prmfir
075400021009     C***                eval      cmzmba = prmfir
075500021009     C***                endif
075600010201     C                   eval      cmznrr = *zeros
075700020305     C                   move      §ctrokvt      cmznrr
075800021018     C                   eval      cmzlba = vlrfl1
075900010201     C                   call(e)   'TIS711C'
076000010201     C                   parm                    dscmz
076100010201     C                   parm      *blanks       esito
076200010205     C                   if        %error
076300010205     C                             or cmzerr = '1'
076400010205     C                             or esito  = '1'
076500010205     C                   eval      wrkesito = '3'
076600010205     C                   else
076700010201     C*
076800021113     C* 2° invio FIVAB
076900100525     C                   if        *in51 = *off
077000010201     C                   reset                   dscmz
077100110411     C                   move      vabfgs        cmzdst
077200010201     C                   eval      cmzfld = vlrfou
077300010201     C                   eval      cmzmbd = vlrhdl
077400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077500021009     C***                if        prmfir = *blanks
077600021113     C                   eval      cmzfla = 'FIVAB00F'
077700021113     C                   eval      cmzmba = 'FIVAB00F'
077800021009     C***                else
077900021009     C***                eval      cmzfla = prmfir
078000021009     C***                eval      cmzmba = prmfir
078100021009     C***                endif
078200010201     C                   eval      cmznrr = *zeros
078300010201     C                   move      §ctrokvb      cmznrr
078400021018     C                   eval      cmzlba = vlrfl1
078500010201     C                   call(e)   'TIS711C'
078600010201     C                   parm                    dscmz
078700010201     C                   parm      *blanks       esito
078800010201     C                   if        %error
078900010201     C                             or cmzerr = '1'
079000010201     C                             or esito  = '1'
079100010201     C                   eval      wrkesito = '3'
079200010201     C                   endif
079300100525     C                   endif
079400010205     C                   endif
079500990920     C*
079600000613     C                   ENDSR
079700000613     C***
079800070411
079900110627     C     *pssr         BEGSR
080000070411     C*
080100110627     C                   if        %open(tivin00r)
080200110627     C                   close     tivin00r
080300110627     C                   endif
080400110627     C                   if        %open(fivabwwr)
080500110627     C                   close     fivabwwr
080600110627     C                   endif
080700110627     C                   if        %open(fivatwwr)
080800110627     C                   close     fivatwwr
080900110627     C                   endif
081000110627     C*
081100110627     C* Effettuo la chiamata al CLLE preposto
081200110627     C  N51              call(e)   'TITVVTC'
081300110627     C                   parm                    parccm
081400110627     C                   parm                    parmbr
081500110627     C                   parm      '2'           paropz
081600110627     C   51              call(e)   'TITVVBC'
081700110627     C                   parm                    parccm
081800110627     C                   parm                    parmbr
081900110627     C                   parm      '2'           paropz
082000110627     C*
082100110627     C                   eval      wrkesito = '2'
082200110627     C*
082300110627     C                   seton                                        LR
082400110627     C*
082500110627     C                   ENDSR     '*CANCL'
082600070411     C***
082700070411
082800990910
082900000613     C     *inzsr        BEGSR
083000990910     C*
083100990910     C     *entry        plist
083200990920     C                   parm                    tivlrds
083300990921     C                   parm      wrkesito      esito
083400000724     C                   parm                    prmlit
083500000710     C                   parm                    prmfir
083600000613     C*
083700000830     C* CALCOLA LA DATA CORRENTE
083800091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
083900091223     C                   eval      datcor = %dec(%date() : *ISO)
084000000830     C*
084100000613     C                   ENDSR
084200000613     C***
084300990908
084400110627     Otitv2g5p  E            riepilogo         2
084500990915     O                                              'Upload via Internet'
084600990915     O                                           +1 'Traduzione TIVIN00R -'
084700021113     O                                           +1 'FIVABWWR/FIVATWWR'
084800010201     O                                           +1 'Report testate bolle'
084900990915     O          E            riepilogo   2
085000990915     O                       wrkdata
085100990915     O                       wrkora              +1
085200990915     O                       procname            +1
085300990915     O          E            riepilogo   2
085400990915     O                                              'Cliente..................:'
085500990915     O                       VABCCM        z     +1
085600990915     O          E            riepilogo   2
085700990920     O                                              'Riferimento Strategi.....:'
085800990920     O                       vlrhdl              +1
085900990915     O          E            riepilogo   2
086000990915     O                                              'Giusti...................:'
086100010201     O                       §CTROKVB      2   +  1
086200100525     O                       §CTROKVT      2   +  1
086300990915     O          E            riepilogo   2
086400990915     O                                              'Sbagliati e corretti.....:'
086500971022     O                       §CTRMO        2   +  1
086600990915     O          E            riepilogo   2
086700990915     O                                              'Sbagliati e scartati.....:'
086800971022     O                       §CTRNO        2   +  1
086900000613
087000110627     Otitv2g5ps E            testdett          2
087100000613     O                                              'Upload via Internet'
087200000613     O                                           +1 'Traduzione TIVIN00R -'
087300021113     O                                           +1 'FIVABWWR/FIVATWWR'
087400010201     O                                           +1 'Report testate bolle'
087500000616     O          E            testdett    3
087600000613     O                                           +2 'N° rec'
087700000613     O                                           +3 'Anteprima contenuto'
087800000616     O          E            rigadett    2
087900000613     O                       rrnum               +2
088000000621     O                       recko               +3
088100000616     O          E            findett     2
088200000613     O                       wrkdata
088300000613     O                       wrkora              +1
088400000613     O                       procname            +1
088500000616     O          E            findett     2
088600000613     O                                              'Cliente..................:'
088700000613     O                       VABCCM        z     +1
088800000616     O          E            findett     2
088900000613     O                                              'Riferimento Strategi.....:'
089000000613     O                       vlrhdl              +1
089100000616     O          E            findett     2
089200000613     O                                              'Giusti...................:'
089300010201     O                       §CTROKVB      2   +  1
089400100525     O                       §CTROKVT      2   +  1
089500000616     O          E            findett     2
089600000613     O                                              'Sbagliati e corretti.....:'
089700000613     O                       §CTRMO        2   +  1
089800000616     O          E            findett     2
089900000613     O                                              'Sbagliati e scartati.....:'
090000000613     O                       §CTRNO        2   +  1
090100000512** CMD - COMANDI CL
090200110627OVRPRTF FILE(TITV2G5P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
090300110627OVRPRTF FILE(TITV2G5PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
090400110627DLTOVR FILE(TITV2G5P)  LVL(*)
090500110627DLTOVR FILE(TITV2G5PS) LVL(*)
090600000512
