000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600100412     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800100419     Ftitv1w8p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100100419     Ftitv1w8ps O    f  198        PRINTER usropn
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
003600110110     D depspe          s              8    INZ(*blanks)
003700110110     D curspe          s              8    INZ(*blanks)
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
011600100419     C                   if        not %open(titv1w8ps)
011700100419     C                   open      titv1w8ps
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
013600100419     C                   if        %open(titv1w8ps)
013700000616     C                   except    findett
013800100419     C                   close     titv1w8ps
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
015700100419     C                   if        not %open(titv1w8p)
015800100419     C                   open      titv1w8p
015900990915     C                   endif
016000990915     C*
016100990915     C                   except    riepilogo
016200990915     C*
016300100419     C                   if        %open(titv1w8p)
016400100419     C                   close     titv1w8p
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
020200100419     C+ vindta, vinflg, substr(vindta, 81, 6) as sped, rrn(tivin00r)
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
022900110110     C                   EVAL      curspe=%trim(%subst(vindta:80:8))
023000010305     C*
023100071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
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
037600100419     C                   EVAL      VABCCM = 0492495
037700100419     C                   EVAL      VATCCM = 0492495
037800100419     C                   EVAL      VABLNP = 049
037900100419     C                   EVAL      VATLNP = 049
038000100419     C                   EVAL      VABCTR = 300
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
045900100419     C                   EVAL      VABRSD=%trim(%subst(vindta:155:35))
046000070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
046100070928     C     '@':'A'       XLATE     VABRSD        VABRSD
046200070928     C* ==
046300100419     C                   EVAL      VABIND=%trim(%subst(vindta:225:35))
046400100419     C                   EVAL      VABLOD=%trim(%subst(vindta:260:35))
046500100419     C                   EVAL      VABCAD=%trim(%subst(vindta:315:7))
046600100419     C                   EVAL      VABPRD=%trim(%subst(vindta:322:2))
046700110110     C                   EVAL      VABRMA=%trim(%subst(vindta:80:8))
046800100419     C                   EVAL      VABCBO=%trim(%subst(vindta:12:1))
046900100419     C                   EVAL      VABNOT=%trim(%subst(vindta:461:35))
047000100419     C                   EVAL      VABNT2=%trim(%subst(vindta:461+35:60-35))
047100100419     C* CCM
047200100419     C                   IF        %trim(%subst(vindta:2:7)) = '2100341'
047300100419     C                   EVAL      VABCCM = 2100341
047400100419     C                   EVAL      VATCCM = 2100341
047500100419     C                   EVAL      VABCTR = 000
047600100419     C                   ENDIF
047700100309     C* NCL
047800100419     C                   EVAL      PiStr=%trim(%subst(vindta:353:5))
047900100309     C                   EXSR      CHKNUM
048000100309     C                   IF        PiInt=*on
048100100309     C                   Z-ADD     PiVal         VABNCL
048200100309     C                   ELSE
048300100309     C                   SETON                                        32
048400100309     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048500100309     C                             + ' ' + 'VABNCL'
048600100309     C                   ENDIF
048700100309     C* PKB
048800100419     C                   EVAL      PiStr=%trim(%subst(vindta:358:7))
048900100309     C                   EXSR      CHKNUM
049000100309     C                   IF        PiNum=*on
049100100510     C                   EVAL      PiVal = PiVal / 100                          * gestisco 2 dec.
049200100412     C                   ADD(H)    PiVal         VABPKB
049300100309     C                   ELSE
049400100309     C                   SETON                                        32
049500100309     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049600100309     C                             + ' ' + 'VABPKB'
049700100309     C                   ENDIF
049800100412     C* RMN
049900110110     C                   EVAL      PiStr=%trim(%subst(vindta:80:8))
050000080923     C                   EXSR      CHKNUM
050100080923     C                   IF        PiInt=*on
050200080923     C                   Z-ADD     PiVal         VABRMN
050300080923     C                   ELSE
050400080923     C                   SETON                                        32
050500080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050600080923     C                             + ' ' + 'VABRMN'
050700080923     C                   ENDIF
050800100310     C*
050900100310     C* Se provincia nn valorizzata la reperisco
051000100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
051100100310     C                   IF        VABNZD = *blanks AND
051200100310     C                             VABPRD = *blanks
051300100310     C                   CLEAR                   TISI95DS
051400100310     C                   EVAL      I95TCN = '3'
051500100310     C                   Z-ADD     datcor        I95DAT
051600100310     C                   EVAL      I95NAR = VABNZD
051700100310     C                   EVAL      I95CAP = VABCAD
051800100310     C                   EVAL      I95LOC = VABLOD
051900100310     C                   CALL      'TISI95R'
052000100310     C                   PARM                    TISI95DS
052100100310     C                   EVAL      VABPRD = O95PRV
052200100310     C                   ENDIF
052300070730     C*
052400070730     C* Considerazioni finali su CBO/CAS
052500070730     C                   IF        FlgCAS = '1'
052600070730     C                   IF        VABCBO = '1'
052700070730     C                   EVAL      VABCBO = '4'
052800070730     C                   ENDIF
052900070730     C                   IF        VABCBO = '2'
053000070730     C                   EVAL      VABCBO = '6'
053100070730     C                   ENDIF
053200070730     C                   ENDIF
053300010202     C*
053400000801     C* Ebbene...
053500000801     C                   ADD       1             §CTRMO
053600070530     C                   IF        *in31 <> *zeros OR
053700070530     C                             *in32 <> *zeros
053800000801     C                   ADD       1             §CTRNO
053900000801     C                   EVAL      recko = vindta
054000000801     C                   EXCEPT    rigadett
054100020725     C                   EVAL      x_vinflg = '2'
054200000801     C                   ELSE
054300010201     C                   ADD       1             §CTROKVB
054400000801     C                   ENDIF
054500000801     C*
054600000801     C                   ENDSR
054700100127     C*----------------------------------------------------*
054800100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
054900100127     C*----------------------------------------------------*
055000100127     C     WRIVAT_A      BEGSR
055100100127     C*
055200100127     C* Valorizzo l buffer di scrittura del FIVAT
055300100127     C* - TIPO RECORD "A"
055400100412     C***                eval      VATTRC = 'A'
055500100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
055600100412     C***                exsr      wrivat
055700100127     C*
055800100127     C                   ENDSR
055900010201     C*----------------------------------------------------*
056000071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
056100010201     C*----------------------------------------------------*
056200071003     C     WRIVAT_B      BEGSR
056300010201     C*
056400021113     C* Valorizzo l buffer di scrittura del FIVAT
056500070928     C* - TIPO RECORD "B"
056600100412     C***                eval      VATTRC = 'B'
056700100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
056800100412     C***                exsr      wrivat
056900010201     C*
057000010201     C                   ENDSR
057100071003     C*----------------------------------------------------*
057200071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
057300071003     C*----------------------------------------------------*
057400071003     C     WRIVAT_E      BEGSR
057500071003     C*
057600071003     C* Valorizzo l buffer di scrittura del FIVAT
057700071003     C* - TIPO RECORD "E"
057800100310     C                   eval      VATTRC = 'E'
057900100419     C*
058000100419     C* X ogni collo indicato in testata
058100100419     C                   z-add     1             wColli            5 0
058200100419     C                   dow       wColli <= VABNCL
058300100419     C*
058400100419     C* Valorizzo il buffer di scrittura del FIVAT
058500100419     C                   move(p)   wColli        wColliAlfa        2
058600100504     C                   eval      VATNOT = %trim(%subst(vindta:21:6))+
058700100419     C                                      wColliAlfa
058800100419     C                   exsr      wrivat                                       => scarico VAT
058900100419     C*
059000100419     C                   add       1             wColli
059100100419     C                   enddo
059200100419     C*
059300071003     C*
059400071003     C                   ENDSR
059500100127     C*----------------------------------------------------*
059600100127     C*  SCARICO BUFFER FIVAT
059700100127     C*----------------------------------------------------*
059800100127     C     WRIVAT        BEGSR
059900100127     C*
060000100127     C                   if        vatNOT <> *blanks
060100100127     C                   ADD       1             §CTROKVT
060200100127     C                   write     FIVAT000
060300100127     C                   endif
060400100127     C*
060500100127     C                   ENDSR
060600020904
060700020904
060800010202     C*----------------------------------------------------*
060900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
061000010202     C*----------------------------------------------------*
061100020305     C     PREVAT        BEGSR
061200010202     C*
061300021113     C* Compongo il nome del membro da dare al FIVATWWR
061400010202     C                   eval      parmbr = vlrhdl
061500010202     C                   movel     'M'           parmbr
061600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
061700010202     C                   eval      paropz = '1'
061800010202     C* Effettuo la chiamata al CLLE preposto
061900050128     C                   call(e)   'TITVVTC'
062000010202     C                   parm                    parccm
062100010202     C                   parm                    parmbr
062200010202     C                   parm                    paropz
062300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
062400010202     C                   if        %error
062500010202     C                   movel     '1'           chkcall
062600010202     C                   else
062700010202     C                   movel     '0'           chkcall
062800010202     C                   endif
062900010202     C*
063000010202     C                   ENDSR
063100000801     C*----------------------------------------------------*
063200000801     C*  CONTROLLO NUMERICITA' CAMPI
063300000801     C*----------------------------------------------------*
063400000801     C     CHKNUM        BEGSR
063500081113     C*
063600081113     C                   IF        PiDecChr = *blanks
063700091223     C                   EVAL      PiDecChr = ','
063800081113     C                   ENDIF
063900091223     C*
064000081113     C                   callp(e)  UBISNUM_Check(PiStr
064100081113     C                                          :PiDecChr
064200081113     C                                          :PiVal
064300081113     C                                          :PiNum
064400081113     C                                          :PiInt)
064500081113     C*
064600000801     C                   IF        %error
064700000801     C                   EVAL      PiInt=*off
064800000801     C                   ENDIF
064900000801     C*
065000000801     C                   ENDSR
065100000801     C***
065200011113
065300011113
065400000801
065500000801
065600990920      /TITLE Invio dei dati al punto operativo.
065700010202     C     invio         BEGSR
065800990920     C*
065900021113     C* 1° invio FIVAT
066000010201     C                   reset                   dscmz
066100010201     C                   move      vlrpoi        cmzdst
066200021113     C                   eval      cmzfld = 'FIVATWWR'
066300010201     C                   eval      cmzmbd = vlrhdl
066400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
066500021009     C***                if        prmfir = *blanks
066600021113     C                   eval      cmzfla = 'FIVAT00F'
066700021113     C                   eval      cmzmba = 'FIVAT00F'
066800021009     C***                else
066900021009     C***                eval      cmzfla = prmfir
067000021009     C***                eval      cmzmba = prmfir
067100021009     C***                endif
067200010201     C                   eval      cmznrr = *zeros
067300020305     C                   move      §ctrokvt      cmznrr
067400021018     C                   eval      cmzlba = vlrfl1
067500010201     C                   call(e)   'TIS711C'
067600010201     C                   parm                    dscmz
067700010201     C                   parm      *blanks       esito
067800010205     C                   if        %error
067900010205     C                             or cmzerr = '1'
068000010205     C                             or esito  = '1'
068100010205     C                   eval      wrkesito = '3'
068200010205     C                   else
068300010201     C*
068400021113     C* 2° invio FIVAB
068500010201     C                   reset                   dscmz
068600010201     C                   move      vlrpoi        cmzdst
068700010201     C                   eval      cmzfld = vlrfou
068800010201     C                   eval      cmzmbd = vlrhdl
068900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
069000021009     C***                if        prmfir = *blanks
069100021113     C                   eval      cmzfla = 'FIVAB00F'
069200021113     C                   eval      cmzmba = 'FIVAB00F'
069300021009     C***                else
069400021009     C***                eval      cmzfla = prmfir
069500021009     C***                eval      cmzmba = prmfir
069600021009     C***                endif
069700010201     C                   eval      cmznrr = *zeros
069800010201     C                   move      §ctrokvb      cmznrr
069900021018     C                   eval      cmzlba = vlrfl1
070000010201     C                   call(e)   'TIS711C'
070100010201     C                   parm                    dscmz
070200010201     C                   parm      *blanks       esito
070300010201     C                   if        %error
070400010201     C                             or cmzerr = '1'
070500010201     C                             or esito  = '1'
070600010201     C                   eval      wrkesito = '3'
070700010201     C                   endif
070800010205     C                   endif
070900990920     C*
071000000613     C                   ENDSR
071100000613     C***
071200070411
071300070411     C     *pssr         BEGSR
071400070411     C*
071500070411     C                   if        %open(tivin00r)
071600070411     C                   close     tivin00r
071700070411     C                   endif
071800070411     C                   if        %open(fivabwwr)
071900070411     C                   close     fivabwwr
072000070411     C                   endif
072100070411     C                   if        %open(fivatwwr)
072200070411     C                   close     fivatwwr
072300070411     C                   endif
072400070411     C*
072500070411     C* Effettuo la chiamata al CLLE preposto
072600070411     C                   call(e)   'TITVVTC'
072700070411     C                   parm                    parccm
072800070411     C                   parm                    parmbr
072900070411     C                   parm      '2'           paropz
073000070411     C*
073100070411     C                   eval      wrkesito = '2'
073200070411     C*
073300070411     C                   seton                                        LR
073400070411     C*
073500070411     C                   ENDSR     '*CANCL'
073600070411     C***
073700070411
073800990910
073900000613     C     *inzsr        BEGSR
074000990910     C*
074100990910     C     *entry        plist
074200990920     C                   parm                    tivlrds
074300990921     C                   parm      wrkesito      esito
074400000724     C                   parm                    prmlit
074500000710     C                   parm                    prmfir
074600000613     C*
074700000830     C* CALCOLA LA DATA CORRENTE
074800091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
074900091223     C                   eval      datcor = %dec(%date() : *ISO)
075000000830     C*
075100000613     C                   ENDSR
075200000613     C***
075300990908
075400100419     Otitv1w8p  E            riepilogo         2
075500990915     O                                              'Upload via Internet'
075600990915     O                                           +1 'Traduzione TIVIN00R -'
075700021113     O                                           +1 'FIVABWWR/FIVATWWR'
075800010201     O                                           +1 'Report testate bolle'
075900990915     O          E            riepilogo   2
076000990915     O                       wrkdata
076100990915     O                       wrkora              +1
076200990915     O                       procname            +1
076300990915     O          E            riepilogo   2
076400990915     O                                              'Cliente..................:'
076500990915     O                       VABCCM        z     +1
076600990915     O          E            riepilogo   2
076700990920     O                                              'Riferimento Strategi.....:'
076800990920     O                       vlrhdl              +1
076900990915     O          E            riepilogo   2
077000990915     O                                              'Giusti...................:'
077100010201     O                       §CTROKVB      2   +  1
077200990915     O          E            riepilogo   2
077300990915     O                                              'Sbagliati e corretti.....:'
077400971022     O                       §CTRMO        2   +  1
077500990915     O          E            riepilogo   2
077600990915     O                                              'Sbagliati e scartati.....:'
077700971022     O                       §CTRNO        2   +  1
077800000613
077900100419     Otitv1w8ps E            testdett          2
078000000613     O                                              'Upload via Internet'
078100000613     O                                           +1 'Traduzione TIVIN00R -'
078200021113     O                                           +1 'FIVABWWR/FIVATWWR'
078300010201     O                                           +1 'Report testate bolle'
078400000616     O          E            testdett    3
078500000613     O                                           +2 'N° rec'
078600000613     O                                           +3 'Anteprima contenuto'
078700000616     O          E            rigadett    2
078800000613     O                       rrnum               +2
078900000621     O                       recko               +3
079000000616     O          E            findett     2
079100000613     O                       wrkdata
079200000613     O                       wrkora              +1
079300000613     O                       procname            +1
079400000616     O          E            findett     2
079500000613     O                                              'Cliente..................:'
079600000613     O                       VABCCM        z     +1
079700000616     O          E            findett     2
079800000613     O                                              'Riferimento Strategi.....:'
079900000613     O                       vlrhdl              +1
080000000616     O          E            findett     2
080100000613     O                                              'Giusti...................:'
080200010201     O                       §CTROKVB      2   +  1
080300000616     O          E            findett     2
080400000613     O                                              'Sbagliati e corretti.....:'
080500000613     O                       §CTRMO        2   +  1
080600000616     O          E            findett     2
080700000613     O                                              'Sbagliati e scartati.....:'
080800000613     O                       §CTRNO        2   +  1
080900000512** CMD - COMANDI CL
081000100419OVRPRTF FILE(TITV1W8P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
081100100419OVRPRTF FILE(TITV1W8PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
081200100419DLTOVR FILE(TITV1W8P TITV1W8PS) LVL(*)
081300000512
081400000512
