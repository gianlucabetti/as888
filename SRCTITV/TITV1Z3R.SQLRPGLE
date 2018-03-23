000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600100412     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800100729     Ftitv1z3p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100100729     Ftitv1z3ps O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
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
003600100729     D depspe          s              8    INZ(*blanks)
003700100729     D curspe          s              8    INZ(*blanks)
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
005100100729     D  VINSPE                        8
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
006400081113
006500081113     D*------------------
006600081113     D* LINKING A DEFINIZIONI ESTERNE
006700081113     D*------------------
006800081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
006900090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007000081113
007100990908
007200010201
007300010201
007400080117     C*
007500080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007600080117     C
007700080117     C/EXEC SQL
007800080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
007900080117     C/END-EXEC
008000080117     C
008100990915     C                   time                    wrkdata
008200990915     C                   time                    wrkora
008300000913     C                   reset                   rrnum
008400990921     C                   reset                   esito
008500990921     C                   reset                   wrkesito
008600000724     C*
008700000724     C* SE OCCORRE SPEDIRE IN FILIALE
008800000724     C                   if        vlrpoi <> *zeros
008900000724     C*
009000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009200000724     C     vlrpoi        chain     azorg01l
009300000724     C                   if        %found
009400000616     C                   movel(p)  CMD(1)        depcmd
009500020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009600000616     C*
009700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009800011113     C                   Z-ADD     150           LENGH            15 5
009900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010000000616     C                   PARM                    depcmd
010100000616     C                   PARM                    LENGH
010200000724     C*
010300000724     C                   endif
010400000724     C                   endif
010500000616     C*
010600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010700000616     C                   movel(p)  CMD(2)        depcmd
010800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010900000616     C*
011000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011100011113     C                   Z-ADD     150           LENGH            15 5
011200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011300000616     C                   PARM                    depcmd
011400000616     C                   PARM                    LENGH
011500000616     C*
011600100729     C                   if        not %open(titv1z3ps)
011700100729     C                   open      titv1z3ps
011800000616     C                   except    testdett
011900000613     C                   endif
012000000613     C*
012100100525     C                   EXSR      DEFCAM                                       LETT/SCR. VAB/VAT
012200070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
012300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012400000613     C*
012500010202     C* Effettuo la chiamata al CLLE preposto
012600100525     C  N51              call(e)   'TITVVTC'
012700100525     C                   parm                    parccm
012800100525     C                   parm                    parmbr
012900100525     C                   parm      '2'           paropz
013000100525     C   51              call(e)   'TITVVBC'
013100100525     C                   parm                    parccm
013200100525     C                   parm                    parmbr
013300100525     C                   parm      '2'           paropz
013400070730     C*
013500070730     C* Effettuo lancio TISI95 solo x chiusura
013600070730     C                   CLEAR                   TISI95DS
013700070730     C                   EVAL      I95TLA = 'C'
013800070730     C                   CALL      'TISI95R'
013900070730     C                   PARM                    TISI95DS
014000010202     C*
014100100729     C                   if        %open(titv1z3ps)
014200000616     C                   except    findett
014300100729     C                   close     titv1z3ps
014400000613     C                   endif
014500000616     C*
014600000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014700011113     C                   Z-ADD     150           LENGH            15 5
014800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014900000616     C                   PARM                    CMD(3)
015000000616     C                   PARM                    LENGH
015100000616     C*
015200000801     C
015300010201     C                   seton                                        LR
015400000613
015500100310
015600100310
015700000613     C*--------------------------------------------------------
015800000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015900000613     C*--------------------------------------------------------
016000000621     C     STPR          BEGSR
016100000613     C*
016200100729     C                   if        not %open(titv1z3p)
016300100729     C                   open      titv1z3p
016400990915     C                   endif
016500990915     C*
016600990915     C                   except    riepilogo
016700990915     C*
016800100729     C                   if        %open(titv1z3p)
016900100729     C                   close     titv1z3p
017000990914     C                   endif
017100990910     C*
017200000613     C                   ENDSR
017300000613     C***
017400990908
017500000801
017600910830     C*--------------------------------------------------------
017700070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
017800910830     C*--------------------------------------------------------
017900070530     C     RWFILE        BEGSR
018000990910     C*
018100990914     C                   if        not %open(tivin00r)
018200990908     C                   open      tivin00r
018300990914     C                   endif
018400070530     C*
018500100525     C* Eseguo operazioni di aggiunta nuovo membro file "nn esplicito"
018600100525     C                   exsr      prevasx
018700010201     C*
018800010202     C                   if        chkcall = '0'
018900010202     C*
019000100525     C                   if        not %open(fivabwwr)
019100100525     C                   open      fivabwwr
019200100525     C                   endif
019300100525     C*
019400021113     C                   if        not %open(fivatwwr)
019500021113     C                   open      fivatwwr
019600010201     C                   endif
019700080117     C*
019800080117     C                   EXSR      INZVAR
019900080117     C                   EXSR      DEFCAM
020000990910     C*
020100010201     C                   clear                   §CTROKVB          5 0
020200020305     C                   clear                   §CTROKVT          5 0
020300000801     C                   clear                   §CTRMO            5 0
020400000801     C                   clear                   §CTRNO            5 0
020500990910     C*
020600020725     C/EXEC SQL
020700020725     C+ declare C1 cursor for select
020800100730     C+ vindta, vinflg, substr(vindta, 48, 8) as sped, rrn(tivin00r)
020900060223     C+ from tivin00r
021000060223     C+ order by sped
021100020725     C+ for read only
021200020725     C/END-EXEC
021300020725     C
021400020725     C/EXEC SQL
021500020725     C+ open C1
021600020725     C/END-EXEC
021700020725     C
021800020725     C/EXEC SQL
021900070530     C+ Fetch C1 into :INPUT_DS
022000020725     C/END-EXEC
022100020725     C*
022200020725     C                   dow       sqlcod = *zeros
022300990913     C*
022400100310     C                   if        vindta > *blanks
022500000613     C                   add       1             rrnum
022600000801     C*
022700020725     C                   if        vinflg = *blanks
022800020725     C                             or vinflg = '0'
022900020725     C                             or vinflg = '2'
023000000801     C*
023100020725     C                   clear                   x_vinmsg
023200020725     C                   eval      x_vinflg = '1'
023300010305     C*
023400010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
023500100730     C                   EVAL      curspe=%trim(%subst(vindta:48:8))
023600010305     C*
023700100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
023800080923     C                   if        *in50 = *off
023900071003     C                   exsr      impvab
024000100525     C                   exsr      wrivab
024100100127     C                   exsr      wrivat_a                                     => carico VAT
024200071003     C                   exsr      wrivat_b                                     => carico VAT
024300100525     C   51              exsr      wrivat_e                                     => carico VAT
024400071003     C                   else
024500080923     C*
024600071009     C                   if        wDISK = 'D'
024700091223     C                   exsr      repNSP
024800071009     C                   exsr      impvab
024900100412     C                   exsr      wrivab
025000100127     C                   exsr      wrivat_a                                     => carico VAT
025100071009     C                   exsr      wrivat_b                                     => carico VAT
025200071009     C                   exsr      wrivat_e                                     => carico VAT
025300071009     C                   else
025400071009     C*
025500010305     C                   if        depspe = *blanks                             => 1° giro
025600010305     C                   eval      depspe = curspe                              => memorizz. spediz
025700080117     C                   clear                   tivinds
025800091223     C                   exsr      repNSP
025900020305     C                   exsr      impvab
026000100127     C                   exsr      wrivat_a                                     => carico VAT
026100071003     C                   exsr      wrivat_b                                     => carico VAT
026200071003     C   50              exsr      wrivat_e                                     => carico VAT
026300010305     C                   else
026400020725     C                   if        depspe <> curspe                             => rottura di spediz
026500010305     C                   eval      depspe = curspe                              => memorizz. spediz
026600100412     C                   exsr      wrivab
026700080117     C                   clear                   tivinds
026800091223     C                   exsr      repNSP
026900020305     C                   exsr      impvab
027000100127     C                   exsr      wrivat_a                                     => carico VAT
027100071003     C                   exsr      wrivat_b                                     => carico VAT
027200071003     C   50              exsr      wrivat_e                                     => carico VAT
027300020305     C                   else                                                   => x stessa spediz
027400100401     C*
027500100401     C* Se nn richiesta esclusione spedizioni "duplicate"
027600100412     C                   if        wDUPREC <> 'N'
027700020305     C                   exsr      impvab
027800100127     C                   exsr      wrivat_a                                     => carico VAT
027900071003     C                   exsr      wrivat_b                                     => carico VAT
028000071003     C   50              exsr      wrivat_e                                     => carico VAT
028100100401     C                   endif
028200010305     C                   endif
028300010305     C                   endif
028400010305     C                   endif
028500071003     C                   endif
028600071009     C                   endif
028700000905     C*
028800000905     C                   else
028900020725     C                   eval      x_vinflg = '1'
029000000905     C                   endif
029100000905     C*
029200020725     C     VINRRN        chain     tivin000
029300020725     C                   if        %found(tivin00r)
029400020725     C                   eval      y_vinflg = x_vinflg
029500020725     C                   eval      y_vinmsg = x_vinmsg
029600020725     C                   update    tivin000
029700020725     C                   endif
029800020725     C*
029900020725     C/EXEC SQL
030000070530     C+ Fetch C1 into :INPUT_DS
030100020725     C/END-EXEC
030200020725     C*
030300020725     C                   enddo
030400020725     C*
030500020725     C/EXEC SQL
030600020725     C+ close C1
030700020725     C/END-EXEC
030800000905     C*
030900020305     C* Scarico i VAB rimasti "in sospeso"
031000071009     C                   if        wDISK = 'C'
031100100412     C                   exsr      wrivab
031200071009     C                   endif
031300010202     C*
031400010202     C                   endif
031500990910
031600990910     C* Se non ci sono record con errori ...
031700000710     C                   if        §ctrno = 0
031800990910     C* ... restituisco esito OK.
031900990921     C                   eval      wrkesito = '0'
032000990910     C                   else
032100100525     C                   if        §ctrokvb > 0 OR
032200100525     C                             §ctrokvt > 0
032300990921     C                   eval      wrkesito = '1'
032400000710     C                   else
032500000710     C                   eval      wrkesito = '2'
032600990910     C                   endif
032700000710     C                   endif
032800990910     C*
032900990914     C                   if        %open(tivin00r)
033000990908     C                   close     tivin00r
033100990914     C                   endif
033200021113     C                   if        %open(fivabwwr)
033300021113     C                   close     fivabwwr
033400990914     C                   endif
033500021113     C                   if        %open(fivatwwr)
033600021113     C                   close     fivatwwr
033700010201     C                   endif
033800990910     C*
033900100525     C                   if        §ctrokvb > 0 OR
034000100525     C                             §ctrokvt > 0
034100000724     C                             and vlrpoi <> *zeros
034200010202     C                   exsr      invio
034300990920     C                   endif
034400990920     C*
034500910830     C                   ENDSR
034600000613     C***
034700100412
034800100412
034900010305
035000010305     C*----------------------------------------------------*
035100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
035200010305     C*----------------------------------------------------*
035300020305     C     WRIVAB        BEGSR
035400100412     C*
035500100412     C* Gestisco l'eventuale rottura x numero spedizione
035600100412     C                   if        VABRMA = *blanks
035700100412     C                   movel(P)  VABRMN        VABRMA
035800100412     C                   endif
035900100412     C*
036000100525     C                   if        *in51 = *off and *in31 = *off
036100100525     C                   write     fivab000                                     => scarico il VAB
036200100525     C                   endif
036300010305     C*
036400010305     C                   ENDSR
036500990920
036600000801     C*----------------------------------------------------*
036700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
036800000801     C*----------------------------------------------------*
036900010201     C     INZVAR        BEGSR
037000000801     C*
037100010201     C                   Z-ADD     *zeros        Num5_0
037200020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
037300020725     C                   MOVEL     '0'           FlgCAS            1
037400000801     C*
037500000801     C                   ENDSR
037600000801     C*----------------------------------------------------*
037700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
037800000801     C*----------------------------------------------------*
037900020904     C     DEFCAM        BEGSR
038000080923     C*
038100100525     C                   SETOFF                                       5051
038200000801     C*
038300021113     C                   CLEAR                   FIVAB000
038400021113     C                   CLEAR                   FIVAT000
038500070730     C* Imposto i valori di default...
038600100729     C                   EVAL      VABCCM = 0494263
038700100729     C                   EVAL      VATCCM = 0494263
038800100729     C                   EVAL      VABLNP = 049
038900100729     C                   EVAL      VATLNP = 049
039000100729     C                   EVAL      VABCTR = 001
039100100525     C                   EVAL      VABCBO = '1'
039200070222     C* ... e poi verifico se sono stati passati come parametri
039300070222     C                   IF        vlrppt > *blanks
039400100525     C*
039500070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
039600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
039700070222     C                   EXSR      CHKNUM
039800070222     C                   IF        PiInt=*on
039900070222     C                   Z-ADD     PiVal         VABCCM
040000070222     C                   Z-ADD     PiVal         VATCCM
040100070222     C                   ENDIF
040200070222     C                   ENDIF
040300070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
040400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
040500070222     C                   EXSR      CHKNUM
040600070222     C                   IF        PiInt=*on
040700070222     C                   Z-ADD     PiVal         VABLNP
040800070222     C                   Z-ADD     PiVal         VATLNP
040900070222     C                   ENDIF
041000070222     C                   ENDIF
041100070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
041200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
041300070222     C                   EXSR      CHKNUM
041400070222     C                   IF        PiInt=*on
041500070222     C                   Z-ADD     PiVal         VABCTR
041600070222     C                   ENDIF
041700070928     C                   ENDIF
041800071009     C                   MOVEL     *blanks       wDISK             1
041900071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
042000071009     C                   IF        wDISK <> *blanks
042100070928     C                   SETON                                        50
042200070222     C                   ENDIF
042300100525     C                   IF        wDISK  = 'T'
042400100525     C                   SETOFF                                       50
042500100525     C                   SETON                                        51
042600100525     C                   ENDIF
042700100401     C                   MOVEL     *blanks       wDUPREC           1
042800100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
042900100525     C                   IF        %subst(vlrppt:16:2) <> *blanks
043000100525     C                   EVAL      VABCTM=%trim(%subst(vlrppt:16:2))
043100100525     C                   ENDIF
043200100525     C*
043300070222     C                   ENDIF
043400071009     C*
043500071009     C   50              EVAL      VABCTM = '7Q'
043600000801     C*
043700000801     C                   ENDSR
043800091223     C*----------------------------------------------------*
043900091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
044000091223     C*----------------------------------------------------*
044100091223     C     REPNSP        BEGSR
044200091223     C*
044300091223     C                   EXSR      INZVAR
044400091223     C                   EXSR      DEFCAM
044500091223     C*
044600091223     C* NSP => Stacco un numeratore da AZNUM
044700091223     C                   clear                   TRUL33DS
044800091223     C                   eval      I33OPE = *zeros
044900091223     C                   eval      I33CNU = 302
045000091223     C                   eval      I33NUM = 1
045100091223     C                   movel     TRUL33DS      KPJBU
045200091223     C                   call      'TRUL33R'
045300091223     C                   parm                    KPJBA
045400091223     C                   movel     KPJBU         TRUL33DS
045500091223     C                   if        O33ERR = *zeros
045600091223     C                   z-add     O33NRF        VABNSP
045700091223     C                   z-add     O33NRF        VATNSP
045800091223     C                   else
045900091223     C                   SETON                                        31
046000091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046100091223     C                             + ' ' + 'VABNSP VATNSP'
046200091223     C                   endif
046300091223     C*
046400091223     C                   ENDSR
046500000801     C*----------------------------------------------------*
046600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
046700000801     C*----------------------------------------------------*
046800100730     C     IMPVAB        BEGSR
046900070530     C*
047000070530     C                   SETOFF                                       3132
047100070928     C*
047200070928     C                   MOVE(P)   vlrpoi        VABFGS
047300070928     C                   MOVE(P)   vlrpoi        VATFGS
047400070928     C*
047500070928     C                   MOVEL     datcor        VABAAS
047600070928     C                   MOVEL     datcor        VATAAS
047700070928     C                   MOVE      datcor        VABMGS
047800100729     C*
047900100729     C* Reperimento campi ALFA
048000100729     C                   EVAL      VABRMO=%trim(%subst(vindta:107:30))
048100100729     C                   EVAL      VABRSD=%trim(%subst(vindta:236:30))
048200100729     C                   EVAL      VABIND=%trim(%subst(vindta:266:30))
048300100729     C                   EVAL      VABLOD=%trim(%subst(vindta:301:25))
048400100729     C                   EVAL      VABPRD=%trim(%subst(vindta:326:2))
048500100729     C                   EVAL      VABRMA=%trim(%subst(vindta:44:12))
048600100730     C                   movel     *blanks       wNOTE            70
048700100730     C                   eval      wNOTE = %trim(%subst(vindta:328:30))+
048800100730     C                                     %trim(%subst(vindta:358:30))+
048900100730     C                                     %trim(%subst(vindta:502:30))+
049000100730     C                                     %trim(%subst(vindta:532:30))
049100100730     C                   EVAL      VABNOT=%subst(wNOTE:1:35)
049200100730     C                   EVAL      VABNT2=%subst(wNOTE:1+35:35)
049300100729     C*
049400100729     C* Considerazioni sulla ragione sociale del destinatario da indicare
049500100729     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
049600100729     C     '@':'A'       XLATE     VABRSD        VABRSD
049700100729     C* ==
049800100729     C*
049900100730     C* CBO
050000100730     C                   SELECT
050100100730     C                   WHEN      %subst(vindta:62:1) = 'A'
050200100730     C                   EVAL      VABCBO = '2'
050300100730     C                   OTHER
050400100730     C                   EVAL      VABCBO = '1'
050500100730     C                   ENDSL
050600100730     C* TIC
050700100730     C                   SELECT
050800100730     C                   WHEN      %subst(vindta:623:1) = '1'
050900100730     C                   EVAL      VABTIC = *blanks
051000100730     C                   WHEN      %subst(vindta:623:1) = '2'
051100100730     C                   EVAL      VABTIC = 'BM'
051200100730     C                   WHEN      %subst(vindta:623:1) = '3'
051300100730     C                   EVAL      VABTIC = 'BM'
051400100730     C                   ENDSL
051500100729     C* CCM/CTR
051600100730     C                   IF        %subst(vindta:44:4) = 'FAZA'
051700100730     C***                EVAL      VABCCM = 0054082
051800100730     C***                EVAL      VABCTR = 100
051900100729     C                   ENDIF
052000100729     C*
052100100729     C* Reperimento campi NUMERICI
052200100729     C                   MOVEL     DATCOR        VABAAS
052300100729     C                   MOVE      DATCOR        VABMGS
052400100730     C* RMN
052500100730     C                   EVAL      PiStr=%trim(%subst(vindta:449:15))
052600100730     C                   EXSR      CHKNUM
052700100730     C                   IF        PiInt=*on
052800100730     C                   Z-ADD     PiVal         VABRMN
052900100730     C                   ELSE
053000100730     C                   SETON                                        32
053100100730     C                   Z-ADD     1             VABRMN
053200100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053300100730     C                             + ' ' + 'VABRMN'
053400100730     C                   ENDIF
053500100729     C* CAD
053600100729     C                   EVAL      PiStr=%trim(%subst(vindta:296:5))
053700100729     C                   EXSR      CHKNUM
053800100729     C                   IF        PiInt=*on
053900100729     C                   Z-ADD     PiVal         Num5_0
054000100729     C                   MOVEL(p)  Num5_0        VABCAD
054100100729     C                   ELSE
054200100729     C                   SETON                                        32
054300100729     C                   EVAL      VABCAD = *zeros
054400100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054500100729     C                             + ' ' + 'VABCAD'
054600100729     C                   ENDIF
054700100729     C* DCR
054800100730     C                   IF        %subst(vindta:443:6) <> *blanks AND
054900100730     C                             %subst(vindta:443:6) <> *zeros
055000100730     C                   EVAL      PiStr='20'+%trim(%subst(vindta:443:6))
055100100729     C                   EXSR      CHKNUM
055200100729     C                   IF        PiInt=*on
055300100729     C                   Z-ADD     PiVal         VABDCR
055400100729     C                   ELSE
055500100729     C                   SETON                                        32
055600100729     C                   Z-ADD     *zeros        VABDCR
055700100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055800100729     C                             + ' ' + 'VABDCR'
055900100729     C                   ENDIF
056000100729     C                   ENDIF
056100100729     C* NCL
056200100729     C                   EVAL      PiStr=%trim(%subst(vindta:400:6))
056300100729     C                   EXSR      CHKNUM
056400100729     C                   IF        PiInt=*on
056500100729     C                   Z-ADD     PiVal         VABNCL
056600100729     C                   ELSE
056700100729     C                   SETON                                        32
056800100729     C                   Z-ADD     *zeros        VABNCL
056900100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057000100729     C                             + ' ' + 'VABNCL'
057100100729     C                   ENDIF
057200100729     C* PKB
057300100729     C                   EVAL      PiStr=%trim(%subst(vindta:406:7))
057400100729     C                   EXSR      CHKNUM
057500100729     C                   IF        PiNum=*on
057600100729     C                   EVAL      PiVal=PiVal/100                              * gestisco 2 dec.
057700100729     C                   Z-ADD(H)  PiVal         VABPKB
057800100729     C                   ELSE
057900100729     C                   SETON                                        32
058000100729     C                   Z-ADD     *zeros        VABPKB
058100100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058200100729     C                             + ' ' + 'VABPKB'
058300100729     C                   ENDIF
058400100730     C* VLB
058500100730     C                   EVAL      PiStr=%trim(%subst(vindta:413:5))
058600100730     C                   EXSR      CHKNUM
058700100730     C                   IF        PiNum=*on
058800100730     C                   EVAL      PiVal=PiVal/1000                             * gestisco 3 dec.
058900100730     C                   Z-ADD(H)  PiVal         VABVLB
059000100730     C                   ELSE
059100100730     C                   SETON                                        32
059200100730     C                   Z-ADD     *zeros        VABVLB
059300100730     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
059400100730     C                             + ' ' + 'VABVLB'
059500100730     C                   ENDIF
059600100729     C* CAS
059700100730     C                   IF        %subst(vindta:63:10)<>'0000000.00'
059800100729     C                   EVAL      FlgCAS = '1'
059900100729     C                   EVAL      VABVCA = 'EUR'
060000100729     C                   EVAL      PiStr=%trim(%subst(vindta:63:10))
060100100729     C                   EXSR      CHKNUM
060200100729     C                   IF        PiNum=*on
060300100729     C                   Z-ADD(H)  PiVal         VABCAS
060400100729     C                   ELSE
060500100729     C                   SETON                                        32
060600100729     C                   Z-ADD     *zeros        VABCAS
060700100729     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
060800100729     C                             + ' ' + 'VABCAS'
060900100729     C                   ENDIF
061000100729     C                   ENDIF
061100100729     C*
061200100729     C* Se provincia nn valorizzata la reperisco
061300100729     C* tramite TISI95R a seconda dei dati d instradamento presenti
061400100729     C                   IF        VABNZD  = *blanks AND
061500100729     C                             VABPRD  = *blanks AND
061600100729     C                             VABCAD <> *blanks
061700100729     C                   CLEAR                   TISI95DS
061800100729     C                   EVAL      I95TCN = '3'
061900100729     C                   Z-ADD     datcor        I95DAT
062000100729     C                   EVAL      I95NAR = VABNZD
062100100729     C                   EVAL      I95CAP = VABCAD
062200100729     C                   EVAL      I95LOC = VABLOD
062300100729     C                   CALL      'TISI95R'
062400100729     C                   PARM                    TISI95DS
062500100729     C                   EVAL      VABPRD = O95PRV
062600100729     C                   ENDIF
062700100729     C*
062800100729     C* Considerazioni finali su CBO/CAS
062900100729     C                   IF        FlgCAS = '1'
063000100729     C                   IF        VABCBO = '1'
063100100729     C                   EVAL      VABCBO = '4'
063200100729     C                   ENDIF
063300100729     C                   IF        VABCBO = '2'
063400100729     C                   EVAL      VABCBO = '6'
063500100729     C                   ENDIF
063600100729     C                   ENDIF
063700010202     C*
063800000801     C* Ebbene...
063900000801     C                   ADD       1             §CTRMO
064000070530     C                   IF        *in31 <> *zeros OR
064100070530     C                             *in32 <> *zeros
064200000801     C                   ADD       1             §CTRNO
064300000801     C                   EVAL      recko = vindta
064400000801     C                   EXCEPT    rigadett
064500020725     C                   EVAL      x_vinflg = '2'
064600000801     C                   ELSE
064700010201     C                   ADD       1             §CTROKVB
064800000801     C                   ENDIF
064900000801     C*
065000000801     C                   ENDSR
065100100127     C*----------------------------------------------------*
065200100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
065300100127     C*----------------------------------------------------*
065400100127     C     WRIVAT_A      BEGSR
065500100127     C*
065600100127     C* Valorizzo l buffer di scrittura del FIVAT
065700100127     C* - TIPO RECORD "A"
065800100412     C***                eval      VATTRC = 'A'
065900100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
066000100412     C***                exsr      wrivat
066100100127     C*
066200100127     C                   ENDSR
066300010201     C*----------------------------------------------------*
066400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
066500010201     C*----------------------------------------------------*
066600071003     C     WRIVAT_B      BEGSR
066700010201     C*
066800021113     C* Valorizzo l buffer di scrittura del FIVAT
066900070928     C* - TIPO RECORD "B"
067000100412     C***                eval      VATTRC = 'B'
067100100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
067200100412     C***                exsr      wrivat
067300010201     C*
067400010201     C                   ENDSR
067500071003     C*----------------------------------------------------*
067600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
067700071003     C*----------------------------------------------------*
067800071003     C     WRIVAT_E      BEGSR
067900071003     C*
068000071003     C* Valorizzo l buffer di scrittura del FIVAT
068100071003     C* - TIPO RECORD "E"
068200100729     C*
068300100729     C                   Z-ADD     1             wCollo            3 0
068400100730     C                   MOVEL     *zeros        wColloAlfa1       8
068500100729     C*
068600100729     C                   EVAL      VATTRC='E'
068700100729     C                   EVAL      wColloAlfa1=%subst(vindta:48:8)
068800100729     C*
068900100729     C* Genero tanti "CHI SONO" quanti sono i colli della spedizione
069000100729     C                   dow       wCollo <= VABNCL and wCollo < 999
069100100730     C                   eval      VATNOT = 'NFAZA'     +
069200100730     C                                      wColloAlfa1 +
069300100730     C                                      %editc(wCollo : 'X')
069400100729     C                   exsr      wrivat                                       => scarico VAT
069500100729     C                   add       1             wCollo
069600100729     C                   enddo
069700071003     C*
069800071003     C                   ENDSR
069900100127     C*----------------------------------------------------*
070000100127     C*  SCARICO BUFFER FIVAT
070100100127     C*----------------------------------------------------*
070200100127     C     WRIVAT        BEGSR
070300100127     C*
070400100127     C                   if        vatNOT <> *blanks
070500100127     C                   ADD       1             §CTROKVT
070600100127     C                   write     FIVAT000
070700100127     C                   endif
070800100127     C*
070900100127     C                   ENDSR
071000020904
071100020904
071200010202     C*----------------------------------------------------*
071300100525     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE "NN ESPLICITO"
071400010202     C*----------------------------------------------------*
071500100525     C     PREVASX       BEGSR
071600100525     C*
071700021113     C* Compongo il nome del membro da dare al FIVATWWR
071800010202     C                   eval      parmbr = vlrhdl
071900010202     C                   movel     'M'           parmbr
072000070530     C                   eval      parccm = %subst(vlrKSC:2:7)
072100010202     C                   eval      paropz = '1'
072200100525     C*
072300010202     C* Effettuo la chiamata al CLLE preposto
072400100525     C  N51              call(e)   'TITVVTC'
072500010202     C                   parm                    parccm
072600010202     C                   parm                    parmbr
072700010202     C                   parm                    paropz
072800100525     C   51              call(e)   'TITVVBC'
072900100525     C                   parm                    parccm
073000100525     C                   parm                    parmbr
073100100525     C                   parm                    paropz
073200100525     C*
073300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
073400010202     C                   if        %error
073500010202     C                   movel     '1'           chkcall
073600010202     C                   else
073700010202     C                   movel     '0'           chkcall
073800010202     C                   endif
073900010202     C*
074000010202     C                   ENDSR
074100000801     C*----------------------------------------------------*
074200000801     C*  CONTROLLO NUMERICITA' CAMPI
074300000801     C*----------------------------------------------------*
074400000801     C     CHKNUM        BEGSR
074500081113     C*
074600081113     C                   IF        PiDecChr = *blanks
074700100503     C                   EVAL      PiDecChr = '.'
074800081113     C                   ENDIF
074900091223     C*
075000081113     C                   callp(e)  UBISNUM_Check(PiStr
075100081113     C                                          :PiDecChr
075200081113     C                                          :PiVal
075300081113     C                                          :PiNum
075400081113     C                                          :PiInt)
075500081113     C*
075600000801     C                   IF        %error
075700000801     C                   EVAL      PiInt=*off
075800000801     C                   ENDIF
075900000801     C*
076000000801     C                   ENDSR
076100000801     C***
076200011113
076300011113
076400000801
076500000801
076600990920      /TITLE Invio dei dati al punto operativo.
076700010202     C     invio         BEGSR
076800990920     C*
076900021113     C* 1° invio FIVAT
077000010201     C                   reset                   dscmz
077100010201     C                   move      vlrpoi        cmzdst
077200021113     C                   eval      cmzfld = 'FIVATWWR'
077300010201     C                   eval      cmzmbd = vlrhdl
077400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077500021009     C***                if        prmfir = *blanks
077600021113     C                   eval      cmzfla = 'FIVAT00F'
077700021113     C                   eval      cmzmba = 'FIVAT00F'
077800021009     C***                else
077900021009     C***                eval      cmzfla = prmfir
078000021009     C***                eval      cmzmba = prmfir
078100021009     C***                endif
078200010201     C                   eval      cmznrr = *zeros
078300020305     C                   move      §ctrokvt      cmznrr
078400021018     C                   eval      cmzlba = vlrfl1
078500010201     C                   call(e)   'TIS711C'
078600010201     C                   parm                    dscmz
078700010201     C                   parm      *blanks       esito
078800010205     C                   if        %error
078900010205     C                             or cmzerr = '1'
079000010205     C                             or esito  = '1'
079100010205     C                   eval      wrkesito = '3'
079200010205     C                   else
079300010201     C*
079400021113     C* 2° invio FIVAB
079500100525     C                   if        *in51 = *off
079600010201     C                   reset                   dscmz
079700010201     C                   move      vlrpoi        cmzdst
079800010201     C                   eval      cmzfld = vlrfou
079900010201     C                   eval      cmzmbd = vlrhdl
080000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
080100021009     C***                if        prmfir = *blanks
080200021113     C                   eval      cmzfla = 'FIVAB00F'
080300021113     C                   eval      cmzmba = 'FIVAB00F'
080400021009     C***                else
080500021009     C***                eval      cmzfla = prmfir
080600021009     C***                eval      cmzmba = prmfir
080700021009     C***                endif
080800010201     C                   eval      cmznrr = *zeros
080900010201     C                   move      §ctrokvb      cmznrr
081000021018     C                   eval      cmzlba = vlrfl1
081100010201     C                   call(e)   'TIS711C'
081200010201     C                   parm                    dscmz
081300010201     C                   parm      *blanks       esito
081400010201     C                   if        %error
081500010201     C                             or cmzerr = '1'
081600010201     C                             or esito  = '1'
081700010201     C                   eval      wrkesito = '3'
081800010201     C                   endif
081900100525     C                   endif
082000010205     C                   endif
082100990920     C*
082200000613     C                   ENDSR
082300000613     C***
082400070411
082500070411     C     *pssr         BEGSR
082600070411     C*
082700070411     C                   if        %open(tivin00r)
082800070411     C                   close     tivin00r
082900070411     C                   endif
083000070411     C                   if        %open(fivabwwr)
083100070411     C                   close     fivabwwr
083200070411     C                   endif
083300070411     C                   if        %open(fivatwwr)
083400070411     C                   close     fivatwwr
083500070411     C                   endif
083600070411     C*
083700070411     C* Effettuo la chiamata al CLLE preposto
083800100525     C  N51              call(e)   'TITVVTC'
083900100525     C                   parm                    parccm
084000100525     C                   parm                    parmbr
084100100525     C                   parm      '2'           paropz
084200100525     C   51              call(e)   'TITVVBC'
084300100525     C                   parm                    parccm
084400100525     C                   parm                    parmbr
084500100525     C                   parm      '2'           paropz
084600070411     C*
084700070411     C                   eval      wrkesito = '2'
084800070411     C*
084900070411     C                   seton                                        LR
085000070411     C*
085100070411     C                   ENDSR     '*CANCL'
085200070411     C***
085300070411
085400990910
085500000613     C     *inzsr        BEGSR
085600990910     C*
085700990910     C     *entry        plist
085800990920     C                   parm                    tivlrds
085900990921     C                   parm      wrkesito      esito
086000000724     C                   parm                    prmlit
086100000710     C                   parm                    prmfir
086200000613     C*
086300000830     C* CALCOLA LA DATA CORRENTE
086400091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
086500091223     C                   eval      datcor = %dec(%date() : *ISO)
086600000830     C*
086700000613     C                   ENDSR
086800000613     C***
086900990908
087000100729     Otitv1z3p  E            riepilogo         2
087100990915     O                                              'Upload via Internet'
087200990915     O                                           +1 'Traduzione TIVIN00R -'
087300021113     O                                           +1 'FIVABWWR/FIVATWWR'
087400010201     O                                           +1 'Report testate bolle'
087500990915     O          E            riepilogo   2
087600990915     O                       wrkdata
087700990915     O                       wrkora              +1
087800990915     O                       procname            +1
087900990915     O          E            riepilogo   2
088000990915     O                                              'Cliente..................:'
088100990915     O                       VABCCM        z     +1
088200990915     O          E            riepilogo   2
088300990920     O                                              'Riferimento Strategi.....:'
088400990920     O                       vlrhdl              +1
088500990915     O          E            riepilogo   2
088600990915     O                                              'Giusti...................:'
088700010201     O                       §CTROKVB      2   +  1
088800100525     O                       §CTROKVT      2   +  1
088900990915     O          E            riepilogo   2
089000990915     O                                              'Sbagliati e corretti.....:'
089100971022     O                       §CTRMO        2   +  1
089200990915     O          E            riepilogo   2
089300990915     O                                              'Sbagliati e scartati.....:'
089400971022     O                       §CTRNO        2   +  1
089500000613
089600100729     Otitv1z3ps E            testdett          2
089700000613     O                                              'Upload via Internet'
089800000613     O                                           +1 'Traduzione TIVIN00R -'
089900021113     O                                           +1 'FIVABWWR/FIVATWWR'
090000010201     O                                           +1 'Report testate bolle'
090100000616     O          E            testdett    3
090200000613     O                                           +2 'N° rec'
090300000613     O                                           +3 'Anteprima contenuto'
090400000616     O          E            rigadett    2
090500000613     O                       rrnum               +2
090600000621     O                       recko               +3
090700000616     O          E            findett     2
090800000613     O                       wrkdata
090900000613     O                       wrkora              +1
091000000613     O                       procname            +1
091100000616     O          E            findett     2
091200000613     O                                              'Cliente..................:'
091300000613     O                       VABCCM        z     +1
091400000616     O          E            findett     2
091500000613     O                                              'Riferimento Strategi.....:'
091600000613     O                       vlrhdl              +1
091700000616     O          E            findett     2
091800000613     O                                              'Giusti...................:'
091900010201     O                       §CTROKVB      2   +  1
092000100525     O                       §CTROKVT      2   +  1
092100000616     O          E            findett     2
092200000613     O                                              'Sbagliati e corretti.....:'
092300000613     O                       §CTRMO        2   +  1
092400000616     O          E            findett     2
092500000613     O                                              'Sbagliati e scartati.....:'
092600000613     O                       §CTRNO        2   +  1
092700000512** CMD - COMANDI CL
092800100729OVRPRTF FILE(TITV1Z3P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
092900100729OVRPRTF FILE(TITV1Z3PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
093000100729DLTOVR FILE(TITV1Z3P TITV1Z3PS) LVL(*)
093100000512
093200000512
