000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600100412     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800100503     Ftitv1x3p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100100503     Ftitv1x3ps O    f  198        PRINTER usropn
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
003600100503     D depspe          s             16    INZ(*blanks)
003700100503     D curspe          s             16    INZ(*blanks)
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
004900020725     D  VINDTA                 1   2048
005000020725     D  VINFLG              2049   2049
005100020725     D  VINSPE              2050   2059
005200020725     D  VINRRN              2060   2067  0
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
011600100503     C                   if        not %open(titv1x3ps)
011700100503     C                   open      titv1x3ps
011800000616     C                   except    testdett
011900000613     C                   endif
012000000613     C*
012100070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
012200000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012300000613     C*
012400010202     C* Effettuo la chiamata al CLLE preposto
012500050128     C                   call(e)   'TITVVTC'
012600010202     C                   parm                    parccm
012700010202     C                   parm                    parmbr
012800010202     C                   parm      '2'           paropz
012900070730     C*
013000070730     C* Effettuo lancio TISI95 solo x chiusura
013100070730     C                   CLEAR                   TISI95DS
013200070730     C                   EVAL      I95TLA = 'C'
013300070730     C                   CALL      'TISI95R'
013400070730     C                   PARM                    TISI95DS
013500010202     C*
013600100503     C                   if        %open(titv1x3ps)
013700000616     C                   except    findett
013800100503     C                   close     titv1x3ps
013900000613     C                   endif
014000000616     C*
014100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014200011113     C                   Z-ADD     150           LENGH            15 5
014300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014400000616     C                   PARM                    CMD(3)
014500000616     C                   PARM                    LENGH
014600000616     C*
014700000801     C
014800010201     C                   seton                                        LR
014900000613
015000100310
015100100310
015200000613     C*--------------------------------------------------------
015300000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015400000613     C*--------------------------------------------------------
015500000621     C     STPR          BEGSR
015600000613     C*
015700100503     C                   if        not %open(titv1x3p)
015800100503     C                   open      titv1x3p
015900990915     C                   endif
016000990915     C*
016100990915     C                   except    riepilogo
016200990915     C*
016300100503     C                   if        %open(titv1x3p)
016400100503     C                   close     titv1x3p
016500990914     C                   endif
016600990910     C*
016700000613     C                   ENDSR
016800000613     C***
016900990908
017000000801
017100910830     C*--------------------------------------------------------
017200070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
017300910830     C*--------------------------------------------------------
017400070530     C     RWFILE        BEGSR
017500990910     C*
017600990914     C                   if        not %open(tivin00r)
017700990908     C                   open      tivin00r
017800990914     C                   endif
017900021113     C                   if        not %open(fivabwwr)
018000021113     C                   open      fivabwwr
018100990914     C                   endif
018200070530     C*
018300021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
018400020305     C                   exsr      prevat
018500010201     C*
018600010202     C                   if        chkcall = '0'
018700010202     C*
018800021113     C                   if        not %open(fivatwwr)
018900021113     C                   open      fivatwwr
019000010201     C                   endif
019100080117     C*
019200080117     C                   EXSR      INZVAR
019300080117     C                   EXSR      DEFCAM
019400990910     C*
019500010201     C                   clear                   §CTROKVB          5 0
019600020305     C                   clear                   §CTROKVT          5 0
019700000801     C                   clear                   §CTRMO            5 0
019800000801     C                   clear                   §CTRNO            5 0
019900990910     C*
020000020725     C/EXEC SQL
020100020725     C+ declare C1 cursor for select
020200100503     C+ vindta, vinflg, substr(vindta, 315, 16) as sped, rrn(tivin00r)
020300060223     C+ from tivin00r
020400060223     C+ order by sped
020500020725     C+ for read only
020600020725     C/END-EXEC
020700020725     C
020800020725     C/EXEC SQL
020900020725     C+ open C1
021000020725     C/END-EXEC
021100020725     C
021200020725     C/EXEC SQL
021300070530     C+ Fetch C1 into :INPUT_DS
021400020725     C/END-EXEC
021500020725     C*
021600020725     C                   dow       sqlcod = *zeros
021700990913     C*
021800100310     C                   if        vindta > *blanks
021900000613     C                   add       1             rrnum
022000000801     C*
022100020725     C                   if        vinflg = *blanks
022200020725     C                             or vinflg = '0'
022300020725     C                             or vinflg = '2'
022400000801     C*
022500020725     C                   clear                   x_vinmsg
022600020725     C                   eval      x_vinflg = '1'
022700010305     C*
022800010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
022900100503     C                   EVAL      curspe=%trim(%subst(vindta:315:16))
023000010305     C*
023100100503     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
023200080923     C                   if        *in50 = *off
023300091223     C                   exsr      repNSP
023400071003     C                   exsr      impvab
023500100412     C                   exsr      wrivab
023600100127     C                   exsr      wrivat_a                                     => carico VAT
023700071003     C                   exsr      wrivat_b                                     => carico VAT
023800071003     C                   else
023900080923     C*
024000071009     C                   if        wDISK = 'D'
024100091223     C                   exsr      repNSP
024200071009     C                   exsr      impvab
024300100412     C                   exsr      wrivab
024400100127     C                   exsr      wrivat_a                                     => carico VAT
024500071009     C                   exsr      wrivat_b                                     => carico VAT
024600071009     C                   exsr      wrivat_e                                     => carico VAT
024700071009     C                   else
024800071009     C*
024900010305     C                   if        depspe = *blanks                             => 1° giro
025000010305     C                   eval      depspe = curspe                              => memorizz. spediz
025100080117     C                   clear                   tivinds
025200091223     C                   exsr      repNSP
025300020305     C                   exsr      impvab
025400100127     C                   exsr      wrivat_a                                     => carico VAT
025500071003     C                   exsr      wrivat_b                                     => carico VAT
025600071003     C   50              exsr      wrivat_e                                     => carico VAT
025700010305     C                   else
025800020725     C                   if        depspe <> curspe                             => rottura di spediz
025900010305     C                   eval      depspe = curspe                              => memorizz. spediz
026000100412     C                   exsr      wrivab
026100080117     C                   clear                   tivinds
026200091223     C                   exsr      repNSP
026300020305     C                   exsr      impvab
026400100127     C                   exsr      wrivat_a                                     => carico VAT
026500071003     C                   exsr      wrivat_b                                     => carico VAT
026600071003     C   50              exsr      wrivat_e                                     => carico VAT
026700020305     C                   else                                                   => x stessa spediz
026800100401     C*
026900100401     C* Se nn richiesta esclusione spedizioni "duplicate"
027000100412     C                   if        wDUPREC <> 'N'
027100020305     C                   exsr      impvab
027200100127     C                   exsr      wrivat_a                                     => carico VAT
027300071003     C                   exsr      wrivat_b                                     => carico VAT
027400071003     C   50              exsr      wrivat_e                                     => carico VAT
027500100401     C                   endif
027600010305     C                   endif
027700010305     C                   endif
027800010305     C                   endif
027900071003     C                   endif
028000071009     C                   endif
028100000905     C*
028200000905     C                   else
028300020725     C                   eval      x_vinflg = '1'
028400000905     C                   endif
028500000905     C*
028600020725     C     VINRRN        chain     tivin000
028700020725     C                   if        %found(tivin00r)
028800020725     C                   eval      y_vinflg = x_vinflg
028900020725     C                   eval      y_vinmsg = x_vinmsg
029000020725     C                   update    tivin000
029100020725     C                   endif
029200020725     C*
029300020725     C/EXEC SQL
029400070530     C+ Fetch C1 into :INPUT_DS
029500020725     C/END-EXEC
029600020725     C*
029700020725     C                   enddo
029800020725     C*
029900020725     C/EXEC SQL
030000020725     C+ close C1
030100020725     C/END-EXEC
030200000905     C*
030300020305     C* Scarico i VAB rimasti "in sospeso"
030400071009     C                   if        wDISK = 'C'
030500100412     C                   exsr      wrivab
030600071009     C                   endif
030700010202     C*
030800010202     C                   endif
030900990910
031000990910     C* Se non ci sono record con errori ...
031100000710     C                   if        §ctrno = 0
031200990910     C* ... restituisco esito OK.
031300990921     C                   eval      wrkesito = '0'
031400990910     C                   else
031500010201     C                   if        §ctrokvb > 0
031600990921     C                   eval      wrkesito = '1'
031700000710     C                   else
031800000710     C                   eval      wrkesito = '2'
031900990910     C                   endif
032000000710     C                   endif
032100990910     C*
032200990914     C                   if        %open(tivin00r)
032300990908     C                   close     tivin00r
032400990914     C                   endif
032500021113     C                   if        %open(fivabwwr)
032600021113     C                   close     fivabwwr
032700990914     C                   endif
032800021113     C                   if        %open(fivatwwr)
032900021113     C                   close     fivatwwr
033000010201     C                   endif
033100990910     C*
033200010201     C                   if        §ctrokvb > 0
033300000724     C                             and vlrpoi <> *zeros
033400010202     C                   exsr      invio
033500990920     C                   endif
033600990920     C*
033700910830     C                   ENDSR
033800000613     C***
033900100412
034000100412
034100010305
034200010305     C*----------------------------------------------------*
034300020305     C*  SCARICAMENTO BUFFER RECORDS VAB
034400010305     C*----------------------------------------------------*
034500020305     C     WRIVAB        BEGSR
034600100412     C*
034700100412     C* Gestisco l'eventuale rottura x numero spedizione
034800100412     C                   if        VABRMA = *blanks
034900100412     C                   movel(P)  VABRMN        VABRMA
035000100412     C                   endif
035100100412     C*
035200100412     C                   write     fivab000                                     => scarico il VAB
035300010305     C*
035400010305     C                   ENDSR
035500990920
035600000801     C*----------------------------------------------------*
035700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
035800000801     C*----------------------------------------------------*
035900010201     C     INZVAR        BEGSR
036000000801     C*
036100010201     C                   Z-ADD     *zeros        Num5_0
036200020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
036300020725     C                   MOVEL     '0'           FlgCAS            1
036400000801     C*
036500000801     C                   ENDSR
036600000801     C*----------------------------------------------------*
036700000801     C*  IMPOSTAZIONE CAMPI COSTANTI
036800000801     C*----------------------------------------------------*
036900020904     C     DEFCAM        BEGSR
037000080923     C*
037100090310     C                   SETOFF                                       50
037200000801     C*
037300021113     C                   CLEAR                   FIVAB000
037400021113     C                   CLEAR                   FIVAT000
037500070730     C* Imposto i valori di default...
037600100503     C                   EVAL      VABCCM = 0434895
037700100503     C                   EVAL      VATCCM = 0434895
037800100503     C                   EVAL      VABLNP = 043
037900100503     C                   EVAL      VATLNP = 043
038000100503     C                   EVAL      VABCTR = 000
038100070730     C                   EVAL      VABCBO = '1'
038200070222     C* ... e poi verifico se sono stati passati come parametri
038300070222     C                   IF        vlrppt > *blanks
038400070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
038500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
038600070222     C                   EXSR      CHKNUM
038700070222     C                   IF        PiInt=*on
038800070222     C                   Z-ADD     PiVal         VABCCM
038900070222     C                   Z-ADD     PiVal         VATCCM
039000070222     C                   ENDIF
039100070222     C                   ENDIF
039200070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
039300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
039400070222     C                   EXSR      CHKNUM
039500070222     C                   IF        PiInt=*on
039600070222     C                   Z-ADD     PiVal         VABLNP
039700070222     C                   Z-ADD     PiVal         VATLNP
039800070222     C                   ENDIF
039900070222     C                   ENDIF
040000070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
040100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
040200070222     C                   EXSR      CHKNUM
040300070222     C                   IF        PiInt=*on
040400070222     C                   Z-ADD     PiVal         VABCTR
040500070222     C                   ENDIF
040600070928     C                   ENDIF
040700071009     C                   MOVEL     *blanks       wDISK             1
040800071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
040900071009     C                   IF        wDISK <> *blanks
041000070928     C                   SETON                                        50
041100070222     C                   ENDIF
041200100401     C                   MOVEL     *blanks       wDUPREC           1
041300100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
041400070222     C                   ENDIF
041500071009     C*
041600071009     C   50              EVAL      VABCTM = '7Q'
041700000801     C*
041800000801     C                   ENDSR
041900091223     C*----------------------------------------------------*
042000091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
042100091223     C*----------------------------------------------------*
042200091223     C     REPNSP        BEGSR
042300091223     C*
042400091223     C                   EXSR      INZVAR
042500091223     C                   EXSR      DEFCAM
042600091223     C*
042700091223     C* NSP => Stacco un numeratore da AZNUM
042800091223     C                   clear                   TRUL33DS
042900091223     C                   eval      I33OPE = *zeros
043000091223     C                   eval      I33CNU = 302
043100091223     C                   eval      I33NUM = 1
043200091223     C                   movel     TRUL33DS      KPJBU
043300091223     C                   call      'TRUL33R'
043400091223     C                   parm                    KPJBA
043500091223     C                   movel     KPJBU         TRUL33DS
043600091223     C                   if        O33ERR = *zeros
043700091223     C                   z-add     O33NRF        VABNSP
043800091223     C                   z-add     O33NRF        VATNSP
043900091223     C                   else
044000091223     C                   SETON                                        31
044100091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044200091223     C                             + ' ' + 'VABNSP VATNSP'
044300091223     C                   endif
044400091223     C*
044500091223     C                   ENDSR
044600000801     C*----------------------------------------------------*
044700021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
044800000801     C*----------------------------------------------------*
044900010201     C     IMPVAB        BEGSR
045000070530     C*
045100070530     C                   SETOFF                                       3132
045200070928     C*
045300070928     C                   MOVE(P)   vlrpoi        VABFGS
045400070928     C                   MOVE(P)   vlrpoi        VATFGS
045500070928     C*
045600070928     C                   MOVEL     datcor        VABAAS
045700070928     C                   MOVEL     datcor        VATAAS
045800070928     C                   MOVE      datcor        VABMGS
045900100503     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
046000100503     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
046100100503     C     '@':'A'       XLATE     VABRSD        VABRSD
046200100503     C* ==
046300100503     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
046400100503     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
046500100503     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
046600100503     C                   EVAL      VABRMA=%trim(%subst(vindta:315:16))
046700100503     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
046800100503     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
046900100503     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
047000100503     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
047100100503     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
047200100503     C* DCR
047300100503     C                   IF        %trim(%subst(vindta:197:14))='DATA CONSEGNA:'
047400100503     C                   MOVEL     *blanks       wDCR              8
047500100503     C                   EVAL      wDCR = %subst(vindta:218:4)+
047600100503     C                                    %subst(vindta:215:2)+
047700100531     C                                    %subst(vindta:212:2)
047800100503     C                   MOVE(P)   wDCR          VABDCR
047900100503     C                   ENDIF
048000100503     C* CBO
048100100503     C                   IF        %trim(%subst(vindta:390:1)) = 'F'
048200100503     C                   EVAL      VABCBO='1'
048300100503     C                   ENDIF
048400100503     C                   IF        %trim(%subst(vindta:390:1)) = 'A'
048500100503     C                   EVAL      VABCBO='2'
048600100503     C                   ENDIF
048700100503     C* TSP
048800100503     C                   IF        %trim(%subst(vindta:12:1)) = 'D'
048900100503     C                   EVAL      VABTSP='E'
049000100503     C                   ENDIF
049100100503     C* FFD / TC1 / TC2
049200100503     C                   IF        %trim(%subst(vindta:16:1)) = '1'
049300100503     C                   EVAL      VABFFD='S'
049400100503     C                   ENDIF
049500100503     C                   IF        %trim(%subst(vindta:16:1)) = '2'
049600100503     C                   EVAL      VABTC1='A'
049700100503     C                   ENDIF
049800100503     C* CAD
049900100503     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
050000100503     C                   EXSR      CHKNUM
050100100503     C                   IF        PiInt=*on
050200100503     C                   Z-ADD     PiVal         Num5_0
050300100503     C                   MOVEL     Num5_0        VABCAD
050400100503     C                   ELSE
050500100503     C                   SETON                                        32
050600100503     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050700100503     C                             + ' ' + 'VABCAD'
050800100503     C                   ENDIF
050900100503     C* RMN
051000100503     C                   EVAL      PiStr=%trim(%subst(vindta:315:16))
051100100503     C                   EXSR      CHKNUM
051200100503     C                   IF        PiInt=*on
051300100503     C                   Z-ADD     PiVal         VABRMN
051400100503     C                   ELSE
051500100503     C                   SETON                                        32
051600100503     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051700100503     C                             + ' ' + 'VABRMN'
051800100503     C                   ENDIF
051900100503     C* NCL
052000100503     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
052100100503     C                   EXSR      CHKNUM
052200100503     C                   IF        PiInt=*on
052300100503     C                   Z-ADD     PiVal         VABNCL
052400100503     C                   ELSE
052500100503     C                   SETON                                        32
052600100503     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052700100503     C                             + ' ' + 'VABNCL'
052800100503     C                   ENDIF
052900100503     C* PKB
053000100503     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
053100100503     C                   EXSR      CHKNUM
053200100503     C                   IF        PiNum=*on
053300100503     C                   Z-ADD     PiVal         VABPKB
053400100503     C                   ELSE
053500100503     C                   SETON                                        32
053600100503     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053700100503     C                             + ' ' + 'VABPKB'
053800100503     C                   ENDIF
053900100503     C* CAS
054000100503     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
054100100503     C                             AND
054200100503     C                             %trim(%subst(vindta:280:12))<>*zeros
054300100503     C                             AND
054400100503     C                             %trim(%subst(vindta:280:12))<>'0,00'
054500100503     C                             AND
054600100503     C                             %trim(%subst(vindta:280:12))<>'0.00'
054700100503     C                             AND
054800100503     C                             %trim(%subst(vindta:280:12))<>'0,0'
054900100503     C                             AND
055000100503     C                             %trim(%subst(vindta:280:12))<>'0.0'
055100100503     C                             AND
055200100503     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
055300100503     C                             AND
055400100503     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
055500100503     C                   MOVEL     '1'           FlgCAS
055600100503     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
055700100503     C                   EXSR      CHKNUM
055800100503     C                   IF        PiNum=*on
055900100503     C                   Z-ADD     PiVal         VABCAS
056000100503     C                   ELSE
056100100503     C                   SETON                                        32
056200100503     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056300100503     C                             + ' ' + 'VABCAS'
056400100503     C                   ENDIF
056500100503     C                   ENDIF
056600100503     C* IAS
056700100503     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
056800100503     C                             AND
056900100503     C                             %trim(%subst(vindta:295:12))<>*zeros
057000100503     C                             AND
057100100503     C                             %trim(%subst(vindta:295:12))<>'0,00'
057200100503     C                             AND
057300100503     C                             %trim(%subst(vindta:295:12))<>'0.00'
057400100503     C                             AND
057500100503     C                             %trim(%subst(vindta:295:12))<>'0,0'
057600100503     C                             AND
057700100503     C                             %trim(%subst(vindta:295:12))<>'0.0'
057800100503     C                             AND
057900100503     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
058000100503     C                             AND
058100100503     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
058200100503     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
058300100503     C                   EXSR      CHKNUM
058400100503     C                   IF        PiNum=*on
058500100503     C                   Z-ADD     PiVal         VABIAS
058600100503     C                   ELSE
058700100503     C                   SETON                                        32
058800100503     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058900100503     C                             + ' ' + 'VABIAS'
059000100503     C                   ENDIF
059100100503     C                   ENDIF
059200100310     C*
059300100310     C* Se provincia nn valorizzata la reperisco
059400100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
059500100310     C                   IF        VABNZD = *blanks AND
059600100310     C                             VABPRD = *blanks
059700100310     C                   CLEAR                   TISI95DS
059800100310     C                   EVAL      I95TCN = '3'
059900100310     C                   Z-ADD     datcor        I95DAT
060000100310     C                   EVAL      I95NAR = VABNZD
060100100310     C                   EVAL      I95CAP = VABCAD
060200100310     C                   EVAL      I95LOC = VABLOD
060300100310     C                   CALL      'TISI95R'
060400100310     C                   PARM                    TISI95DS
060500100310     C                   EVAL      VABPRD = O95PRV
060600100310     C                   ENDIF
060700070730     C*
060800070730     C* Considerazioni finali su CBO/CAS
060900070730     C                   IF        FlgCAS = '1'
061000070730     C                   IF        VABCBO = '1'
061100070730     C                   EVAL      VABCBO = '4'
061200070730     C                   ENDIF
061300070730     C                   IF        VABCBO = '2'
061400070730     C                   EVAL      VABCBO = '6'
061500070730     C                   ENDIF
061600070730     C                   ENDIF
061700010202     C*
061800000801     C* Ebbene...
061900000801     C                   ADD       1             §CTRMO
062000070530     C                   IF        *in31 <> *zeros OR
062100070530     C                             *in32 <> *zeros
062200000801     C                   ADD       1             §CTRNO
062300000801     C                   EVAL      recko = vindta
062400000801     C                   EXCEPT    rigadett
062500020725     C                   EVAL      x_vinflg = '2'
062600000801     C                   ELSE
062700010201     C                   ADD       1             §CTROKVB
062800000801     C                   ENDIF
062900000801     C*
063000000801     C                   ENDSR
063100100127     C*----------------------------------------------------*
063200100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
063300100127     C*----------------------------------------------------*
063400100127     C     WRIVAT_A      BEGSR
063500100127     C*
063600100127     C* Valorizzo l buffer di scrittura del FIVAT
063700100127     C* - TIPO RECORD "A"
063800100412     C***                eval      VATTRC = 'A'
063900100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
064000100412     C***                exsr      wrivat
064100100127     C*
064200100127     C                   ENDSR
064300010201     C*----------------------------------------------------*
064400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
064500010201     C*----------------------------------------------------*
064600071003     C     WRIVAT_B      BEGSR
064700010201     C*
064800021113     C* Valorizzo l buffer di scrittura del FIVAT
064900070928     C* - TIPO RECORD "B"
065000100412     C***                eval      VATTRC = 'B'
065100100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
065200100412     C***                exsr      wrivat
065300010201     C*
065400010201     C                   ENDSR
065500071003     C*----------------------------------------------------*
065600071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
065700071003     C*----------------------------------------------------*
065800071003     C     WRIVAT_E      BEGSR
065900071003     C*
066000071003     C* Valorizzo l buffer di scrittura del FIVAT
066100071003     C* - TIPO RECORD "E"
066200100310     C                   eval      VATTRC = 'E'
066300100607     C                   eval      VATNOT = %trim(%subst(vindta:315:14))
066400100607     C                   exsr      wrivat                                       => scarico VAT
066500100419     C*
066600100419     C* X ogni collo indicato in testata
066700100607     C***                z-add     1             wColli            5 0
066800100607     C***                dow       wColli <= VABNCL
066900100419     C*
067000100419     C* Valorizzo il buffer di scrittura del FIVAT
067100100607     C***                move(p)   wColli        wColliAlfa        2
067200100503     C***                eval      VATNOT = %trim(%subst(vindta:331:26))+
067300100503     C***                                   wColliAlfa
067400100607     C***                exsr      wrivat                                       => scarico VAT
067500100419     C*
067600100607     C***                add       1             wColli
067700100607     C***                enddo
067800100419     C*
067900071003     C*
068000071003     C                   ENDSR
068100100127     C*----------------------------------------------------*
068200100127     C*  SCARICO BUFFER FIVAT
068300100127     C*----------------------------------------------------*
068400100127     C     WRIVAT        BEGSR
068500100127     C*
068600100127     C                   if        vatNOT <> *blanks
068700100127     C                   ADD       1             §CTROKVT
068800100127     C                   write     FIVAT000
068900100127     C                   endif
069000100127     C*
069100100127     C                   ENDSR
069200020904
069300020904
069400010202     C*----------------------------------------------------*
069500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
069600010202     C*----------------------------------------------------*
069700020305     C     PREVAT        BEGSR
069800010202     C*
069900021113     C* Compongo il nome del membro da dare al FIVATWWR
070000010202     C                   eval      parmbr = vlrhdl
070100010202     C                   movel     'M'           parmbr
070200070530     C                   eval      parccm = %subst(vlrKSC:2:7)
070300010202     C                   eval      paropz = '1'
070400010202     C* Effettuo la chiamata al CLLE preposto
070500050128     C                   call(e)   'TITVVTC'
070600010202     C                   parm                    parccm
070700010202     C                   parm                    parmbr
070800010202     C                   parm                    paropz
070900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
071000010202     C                   if        %error
071100010202     C                   movel     '1'           chkcall
071200010202     C                   else
071300010202     C                   movel     '0'           chkcall
071400010202     C                   endif
071500010202     C*
071600010202     C                   ENDSR
071700000801     C*----------------------------------------------------*
071800000801     C*  CONTROLLO NUMERICITA' CAMPI
071900000801     C*----------------------------------------------------*
072000000801     C     CHKNUM        BEGSR
072100081113     C*
072200081113     C                   IF        PiDecChr = *blanks
072300100503     C                   EVAL      PiDecChr = '.'
072400081113     C                   ENDIF
072500091223     C*
072600081113     C                   callp(e)  UBISNUM_Check(PiStr
072700081113     C                                          :PiDecChr
072800081113     C                                          :PiVal
072900081113     C                                          :PiNum
073000081113     C                                          :PiInt)
073100081113     C*
073200000801     C                   IF        %error
073300000801     C                   EVAL      PiInt=*off
073400000801     C                   ENDIF
073500000801     C*
073600000801     C                   ENDSR
073700000801     C***
073800011113
073900011113
074000000801
074100000801
074200990920      /TITLE Invio dei dati al punto operativo.
074300010202     C     invio         BEGSR
074400990920     C*
074500021113     C* 1° invio FIVAT
074600010201     C                   reset                   dscmz
074700010201     C                   move      vlrpoi        cmzdst
074800021113     C                   eval      cmzfld = 'FIVATWWR'
074900010201     C                   eval      cmzmbd = vlrhdl
075000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075100021009     C***                if        prmfir = *blanks
075200021113     C                   eval      cmzfla = 'FIVAT00F'
075300021113     C                   eval      cmzmba = 'FIVAT00F'
075400021009     C***                else
075500021009     C***                eval      cmzfla = prmfir
075600021009     C***                eval      cmzmba = prmfir
075700021009     C***                endif
075800010201     C                   eval      cmznrr = *zeros
075900020305     C                   move      §ctrokvt      cmznrr
076000021018     C                   eval      cmzlba = vlrfl1
076100010201     C                   call(e)   'TIS711C'
076200010201     C                   parm                    dscmz
076300010201     C                   parm      *blanks       esito
076400010205     C                   if        %error
076500010205     C                             or cmzerr = '1'
076600010205     C                             or esito  = '1'
076700010205     C                   eval      wrkesito = '3'
076800010205     C                   else
076900010201     C*
077000021113     C* 2° invio FIVAB
077100010201     C                   reset                   dscmz
077200010201     C                   move      vlrpoi        cmzdst
077300010201     C                   eval      cmzfld = vlrfou
077400010201     C                   eval      cmzmbd = vlrhdl
077500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
077600021009     C***                if        prmfir = *blanks
077700021113     C                   eval      cmzfla = 'FIVAB00F'
077800021113     C                   eval      cmzmba = 'FIVAB00F'
077900021009     C***                else
078000021009     C***                eval      cmzfla = prmfir
078100021009     C***                eval      cmzmba = prmfir
078200021009     C***                endif
078300010201     C                   eval      cmznrr = *zeros
078400010201     C                   move      §ctrokvb      cmznrr
078500021018     C                   eval      cmzlba = vlrfl1
078600010201     C                   call(e)   'TIS711C'
078700010201     C                   parm                    dscmz
078800010201     C                   parm      *blanks       esito
078900010201     C                   if        %error
079000010201     C                             or cmzerr = '1'
079100010201     C                             or esito  = '1'
079200010201     C                   eval      wrkesito = '3'
079300010201     C                   endif
079400010205     C                   endif
079500990920     C*
079600000613     C                   ENDSR
079700000613     C***
079800070411
079900070411     C     *pssr         BEGSR
080000070411     C*
080100070411     C                   if        %open(tivin00r)
080200070411     C                   close     tivin00r
080300070411     C                   endif
080400070411     C                   if        %open(fivabwwr)
080500070411     C                   close     fivabwwr
080600070411     C                   endif
080700070411     C                   if        %open(fivatwwr)
080800070411     C                   close     fivatwwr
080900070411     C                   endif
081000070411     C*
081100070411     C* Effettuo la chiamata al CLLE preposto
081200070411     C                   call(e)   'TITVVTC'
081300070411     C                   parm                    parccm
081400070411     C                   parm                    parmbr
081500070411     C                   parm      '2'           paropz
081600070411     C*
081700070411     C                   eval      wrkesito = '2'
081800070411     C*
081900070411     C                   seton                                        LR
082000070411     C*
082100070411     C                   ENDSR     '*CANCL'
082200070411     C***
082300070411
082400990910
082500000613     C     *inzsr        BEGSR
082600990910     C*
082700990910     C     *entry        plist
082800990920     C                   parm                    tivlrds
082900990921     C                   parm      wrkesito      esito
083000000724     C                   parm                    prmlit
083100000710     C                   parm                    prmfir
083200000613     C*
083300000830     C* CALCOLA LA DATA CORRENTE
083400091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
083500091223     C                   eval      datcor = %dec(%date() : *ISO)
083600000830     C*
083700000613     C                   ENDSR
083800000613     C***
083900990908
084000100503     Otitv1x3p  E            riepilogo         2
084100990915     O                                              'Upload via Internet'
084200990915     O                                           +1 'Traduzione TIVIN00R -'
084300021113     O                                           +1 'FIVABWWR/FIVATWWR'
084400010201     O                                           +1 'Report testate bolle'
084500990915     O          E            riepilogo   2
084600990915     O                       wrkdata
084700990915     O                       wrkora              +1
084800990915     O                       procname            +1
084900990915     O          E            riepilogo   2
085000990915     O                                              'Cliente..................:'
085100990915     O                       VABCCM        z     +1
085200990915     O          E            riepilogo   2
085300990920     O                                              'Riferimento Strategi.....:'
085400990920     O                       vlrhdl              +1
085500990915     O          E            riepilogo   2
085600990915     O                                              'Giusti...................:'
085700010201     O                       §CTROKVB      2   +  1
085800990915     O          E            riepilogo   2
085900990915     O                                              'Sbagliati e corretti.....:'
086000971022     O                       §CTRMO        2   +  1
086100990915     O          E            riepilogo   2
086200990915     O                                              'Sbagliati e scartati.....:'
086300971022     O                       §CTRNO        2   +  1
086400000613
086500100503     Otitv1x3ps E            testdett          2
086600000613     O                                              'Upload via Internet'
086700000613     O                                           +1 'Traduzione TIVIN00R -'
086800021113     O                                           +1 'FIVABWWR/FIVATWWR'
086900010201     O                                           +1 'Report testate bolle'
087000000616     O          E            testdett    3
087100000613     O                                           +2 'N° rec'
087200000613     O                                           +3 'Anteprima contenuto'
087300000616     O          E            rigadett    2
087400000613     O                       rrnum               +2
087500000621     O                       recko               +3
087600000616     O          E            findett     2
087700000613     O                       wrkdata
087800000613     O                       wrkora              +1
087900000613     O                       procname            +1
088000000616     O          E            findett     2
088100000613     O                                              'Cliente..................:'
088200000613     O                       VABCCM        z     +1
088300000616     O          E            findett     2
088400000613     O                                              'Riferimento Strategi.....:'
088500000613     O                       vlrhdl              +1
088600000616     O          E            findett     2
088700000613     O                                              'Giusti...................:'
088800010201     O                       §CTROKVB      2   +  1
088900000616     O          E            findett     2
089000000613     O                                              'Sbagliati e corretti.....:'
089100000613     O                       §CTRMO        2   +  1
089200000616     O          E            findett     2
089300000613     O                                              'Sbagliati e scartati.....:'
089400000613     O                       §CTRNO        2   +  1
089500000512** CMD - COMANDI CL
089600100503OVRPRTF FILE(TITV1X3P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
089700100503OVRPRTF FILE(TITV1X3PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
089800100503DLTOVR FILE(TITV1X3P TITV1X3PS) LVL(*)
089900000512
090000000512
