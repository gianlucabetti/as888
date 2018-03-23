000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600100412     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800100412     Ftitv1w5p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100100412     Ftitv1w5ps O    f  198        PRINTER usropn
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
003600100412     D depspe          s             15    INZ(*blanks)
003700100412     D curspe          s             15    INZ(*blanks)
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
011600100412     C                   if        not %open(titv1w5ps)
011700100412     C                   open      titv1w5ps
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
013600100412     C                   if        %open(titv1w5ps)
013700000616     C                   except    findett
013800100412     C                   close     titv1w5ps
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
015700100412     C                   if        not %open(titv1w5p)
015800100412     C                   open      titv1w5p
015900990915     C                   endif
016000990915     C*
016100990915     C                   except    riepilogo
016200990915     C*
016300100412     C                   if        %open(titv1w5p)
016400100412     C                   close     titv1w5p
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
020200100412     C+ vindta, vinflg, substr(vindta, 7, 15) as sped, rrn(tivin00r)
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
022900100412     C                   EVAL      curspe=%trim(%subst(vindta:7:15))
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
037600100412     C                   EVAL      VABCCM = 0504682
037700100412     C                   EVAL      VATCCM = 0504682
037800100412     C                   EVAL      VABLNP = 050
037900100412     C                   EVAL      VATLNP = 050
038000100412     C                   EVAL      VABCTR = 002
038100070730     C                   EVAL      VABCBO = '1'
038200100309     C                   EVAL      VABVCA = 'EUR'
038300070222     C* ... e poi verifico se sono stati passati come parametri
038400070222     C                   IF        vlrppt > *blanks
038500070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
038600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
038700070222     C                   EXSR      CHKNUM
038800070222     C                   IF        PiInt=*on
038900070222     C                   Z-ADD     PiVal         VABCCM
039000070222     C                   Z-ADD     PiVal         VATCCM
039100070222     C                   ENDIF
039200070222     C                   ENDIF
039300070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
039400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
039500070222     C                   EXSR      CHKNUM
039600070222     C                   IF        PiInt=*on
039700070222     C                   Z-ADD     PiVal         VABLNP
039800070222     C                   Z-ADD     PiVal         VATLNP
039900070222     C                   ENDIF
040000070222     C                   ENDIF
040100070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
040200070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
040300070222     C                   EXSR      CHKNUM
040400070222     C                   IF        PiInt=*on
040500070222     C                   Z-ADD     PiVal         VABCTR
040600070222     C                   ENDIF
040700070928     C                   ENDIF
040800071009     C                   MOVEL     *blanks       wDISK             1
040900071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
041000071009     C                   IF        wDISK <> *blanks
041100070928     C                   SETON                                        50
041200070222     C                   ENDIF
041300100401     C                   MOVEL     *blanks       wDUPREC           1
041400100401     C                   EVAL      wDUPREC = %subst(vlrppt:15:1)
041500070222     C                   ENDIF
041600071009     C*
041700071009     C   50              EVAL      VABCTM = '7Q'
041800000801     C*
041900000801     C                   ENDSR
042000091223     C*----------------------------------------------------*
042100091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
042200091223     C*----------------------------------------------------*
042300091223     C     REPNSP        BEGSR
042400091223     C*
042500091223     C                   EXSR      INZVAR
042600091223     C                   EXSR      DEFCAM
042700091223     C*
042800091223     C* NSP => Stacco un numeratore da AZNUM
042900091223     C                   clear                   TRUL33DS
043000091223     C                   eval      I33OPE = *zeros
043100091223     C                   eval      I33CNU = 302
043200091223     C                   eval      I33NUM = 1
043300091223     C                   movel     TRUL33DS      KPJBU
043400091223     C                   call      'TRUL33R'
043500091223     C                   parm                    KPJBA
043600091223     C                   movel     KPJBU         TRUL33DS
043700091223     C                   if        O33ERR = *zeros
043800091223     C                   z-add     O33NRF        VABNSP
043900091223     C                   z-add     O33NRF        VATNSP
044000091223     C                   else
044100091223     C                   SETON                                        31
044200091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044300091223     C                             + ' ' + 'VABNSP VATNSP'
044400091223     C                   endif
044500091223     C*
044600091223     C                   ENDSR
044700000801     C*----------------------------------------------------*
044800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
044900000801     C*----------------------------------------------------*
045000010201     C     IMPVAB        BEGSR
045100070530     C*
045200070530     C                   SETOFF                                       3132
045300070928     C*
045400070928     C                   MOVE(P)   vlrpoi        VABFGS
045500070928     C                   MOVE(P)   vlrpoi        VATFGS
045600070928     C*
045700070928     C                   MOVEL     datcor        VABAAS
045800070928     C                   MOVEL     datcor        VATAAS
045900070928     C                   MOVE      datcor        VABMGS
046000100412     C                   EVAL      VABRSD=%trim(%subst(vindta:36:40))
046100070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
046200070928     C     '@':'A'       XLATE     VABRSD        VABRSD
046300070928     C* ==
046400100412     C                   EVAL      VABRD2=%trim(%subst(vindta:76:44))
046500100412     C                   EVAL      VABIND=%trim(%subst(vindta:120:40))
046600100412     C                   EVAL      VABNOT=%trim(%subst(vindta:160:40))
046700100412     C                   EVAL      VABLOD=%trim(%subst(vindta:210:40))
046800100412     C                   EVAL      VABCAD=%trim(%subst(vindta:200:10))
046900100412     C                   EVAL      VABRMA=%trim(%subst(vindta:345:7))
047000100309     C* NCL
047100100412     C                   EVAL      PiStr=%trim(%subst(vindta:253:5))
047200100309     C                   EXSR      CHKNUM
047300100309     C                   IF        PiInt=*on
047400100309     C                   Z-ADD     PiVal         VABNCL
047500100309     C                   ELSE
047600100309     C                   SETON                                        32
047700100309     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047800100309     C                             + ' ' + 'VABNCL'
047900100309     C                   ENDIF
048000100309     C* PKB
048100100412     C                   EVAL      PiStr=%trim(%subst(vindta:266:7))
048200100309     C                   EXSR      CHKNUM
048300100309     C                   IF        PiNum=*on
048400100412     C                   EVAL      PiVal = PiVal / 1000                         * gestisco 3 dec.
048500100412     C                   ADD(H)    PiVal         VABPKB
048600100309     C                   ELSE
048700100309     C                   SETON                                        32
048800100309     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048900100309     C                             + ' ' + 'VABPKB'
049000100309     C                   ENDIF
049100100412     C* VLB
049200100412     C                   EVAL      PiStr=%trim(%subst(vindta:282:5))
049300100412     C                   EXSR      CHKNUM
049400100412     C                   IF        PiNum=*on
049500100412     C                   EVAL      PiVal = PiVal / 10000                        * gestisco 4 dec.
049600100412     C                   ADD(H)    PiVal         VABVLB
049700100412     C                   ELSE
049800100412     C                   SETON                                        32
049900100412     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050000100412     C                             + ' ' + 'VABVLB'
050100100412     C                   ENDIF
050200100412     C* RMN
050300100412     C                   EVAL      PiStr=%trim(%subst(vindta:7:15))
050400080923     C                   EXSR      CHKNUM
050500080923     C                   IF        PiInt=*on
050600080923     C                   Z-ADD     PiVal         VABRMN
050700080923     C                   ELSE
050800080923     C                   SETON                                        32
050900080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051000080923     C                             + ' ' + 'VABRMN'
051100080923     C                   ENDIF
051200100310     C*
051300100310     C* Se provincia nn valorizzata la reperisco
051400100310     C* tramite TISI95R a seconda dei dati d instradamento presenti
051500100310     C                   IF        VABNZD = *blanks AND
051600100310     C                             VABPRD = *blanks
051700100310     C                   CLEAR                   TISI95DS
051800100310     C                   EVAL      I95TCN = '3'
051900100310     C                   Z-ADD     datcor        I95DAT
052000100310     C                   EVAL      I95NAR = VABNZD
052100100310     C                   EVAL      I95CAP = VABCAD
052200100310     C                   EVAL      I95LOC = VABLOD
052300100310     C                   CALL      'TISI95R'
052400100310     C                   PARM                    TISI95DS
052500100310     C                   EVAL      VABPRD = O95PRV
052600100310     C                   ENDIF
052700070730     C*
052800070730     C* Considerazioni finali su CBO/CAS
052900070730     C                   IF        FlgCAS = '1'
053000070730     C                   IF        VABCBO = '1'
053100070730     C                   EVAL      VABCBO = '4'
053200070730     C                   ENDIF
053300070730     C                   IF        VABCBO = '2'
053400070730     C                   EVAL      VABCBO = '6'
053500070730     C                   ENDIF
053600070730     C                   ENDIF
053700020305     C*
053800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
053900011113     C                   EXSR      CHKIMPDIV
054000010202     C*
054100000801     C* Ebbene...
054200000801     C                   ADD       1             §CTRMO
054300070530     C                   IF        *in31 <> *zeros OR
054400070530     C                             *in32 <> *zeros
054500000801     C                   ADD       1             §CTRNO
054600000801     C                   EVAL      recko = vindta
054700000801     C                   EXCEPT    rigadett
054800020725     C                   EVAL      x_vinflg = '2'
054900000801     C                   ELSE
055000010201     C                   ADD       1             §CTROKVB
055100000801     C                   ENDIF
055200000801     C*
055300000801     C                   ENDSR
055400100127     C*----------------------------------------------------*
055500100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
055600100127     C*----------------------------------------------------*
055700100127     C     WRIVAT_A      BEGSR
055800100127     C*
055900100127     C* Valorizzo l buffer di scrittura del FIVAT
056000100127     C* - TIPO RECORD "A"
056100100412     C***                eval      VATTRC = 'A'
056200100412     C***                eval      VATNOT = %trim(%subst(vindta:386:25))
056300100412     C***                exsr      wrivat
056400100127     C*
056500100127     C                   ENDSR
056600010201     C*----------------------------------------------------*
056700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
056800010201     C*----------------------------------------------------*
056900071003     C     WRIVAT_B      BEGSR
057000010201     C*
057100021113     C* Valorizzo l buffer di scrittura del FIVAT
057200070928     C* - TIPO RECORD "B"
057300100412     C***                eval      VATTRC = 'B'
057400100412     C***                eval      VATNOT = %trim(%subst(vindta:414:19))
057500100412     C***                exsr      wrivat
057600010201     C*
057700010201     C                   ENDSR
057800071003     C*----------------------------------------------------*
057900071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
058000071003     C*----------------------------------------------------*
058100071003     C     WRIVAT_E      BEGSR
058200071003     C*
058300071003     C* Valorizzo l buffer di scrittura del FIVAT
058400071003     C* - TIPO RECORD "E"
058500100310     C                   eval      VATTRC = 'E'
058600100412     C                   eval      VATNOT = %trim(%subst(vindta:258:7))
058700100310     C                   exsr      wrivat                                       => scarico VAT
058800071003     C*
058900071003     C                   ENDSR
059000100127     C*----------------------------------------------------*
059100100127     C*  SCARICO BUFFER FIVAT
059200100127     C*----------------------------------------------------*
059300100127     C     WRIVAT        BEGSR
059400100127     C*
059500100127     C                   if        vatNOT <> *blanks
059600100127     C                   ADD       1             §CTROKVT
059700100127     C                   write     FIVAT000
059800100127     C                   endif
059900100127     C*
060000100127     C                   ENDSR
060100020904
060200020904
060300010202     C*----------------------------------------------------*
060400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
060500010202     C*----------------------------------------------------*
060600020305     C     PREVAT        BEGSR
060700010202     C*
060800021113     C* Compongo il nome del membro da dare al FIVATWWR
060900010202     C                   eval      parmbr = vlrhdl
061000010202     C                   movel     'M'           parmbr
061100070530     C                   eval      parccm = %subst(vlrKSC:2:7)
061200010202     C                   eval      paropz = '1'
061300010202     C* Effettuo la chiamata al CLLE preposto
061400050128     C                   call(e)   'TITVVTC'
061500010202     C                   parm                    parccm
061600010202     C                   parm                    parmbr
061700010202     C                   parm                    paropz
061800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
061900010202     C                   if        %error
062000010202     C                   movel     '1'           chkcall
062100010202     C                   else
062200010202     C                   movel     '0'           chkcall
062300010202     C                   endif
062400010202     C*
062500010202     C                   ENDSR
062600000801     C*----------------------------------------------------*
062700000801     C*  CONTROLLO NUMERICITA' CAMPI
062800000801     C*----------------------------------------------------*
062900000801     C     CHKNUM        BEGSR
063000081113     C*
063100081113     C                   IF        PiDecChr = *blanks
063200091223     C                   EVAL      PiDecChr = ','
063300081113     C                   ENDIF
063400091223     C*
063500081113     C                   callp(e)  UBISNUM_Check(PiStr
063600081113     C                                          :PiDecChr
063700081113     C                                          :PiVal
063800081113     C                                          :PiNum
063900081113     C                                          :PiInt)
064000081113     C*
064100000801     C                   IF        %error
064200000801     C                   EVAL      PiInt=*off
064300000801     C                   ENDIF
064400000801     C*
064500000801     C                   ENDSR
064600000801     C***
064700000801
064800011113
064900011113     C*----------------------------------------------------*
065000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
065100011113     C*----------------------------------------------------*
065200011113     C     CHKIMPDIV     BEGSR
065300011113     C*
065400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
065500011113     C                   Z-ADD     *zeros        wrkDec            9 9
065600011113     C*
065700011113     C* Come prima cosa effettuo considerazioni sulla divisa
065800011113     C                   IF        vabIAS > *zeros
065900011113     C                   IF        vabVAS <> 'EUR'
066000011113     C                   EVAL      vabVAS =  'ITL'
066100011113     C                   ENDIF
066200011113     C                   ENDIF
066300011113     C*
066400011113     C                   IF        vabCAS > *zeros
066500011113     C                   IF        vabVCA <> 'EUR'
066600011113     C                   EVAL      vabVCA =  'ITL'
066700011113     C                   ENDIF
066800011113     C                   ENDIF
066900011113     C*
067000011113     C                   IF        vabVMD > *zeros
067100020305     C                   IF        vabVAD <> 'EUR'
067200011113     C                   EVAL      vabVAD =  'ITL'
067300011113     C                   ENDIF
067400011113     C                   ENDIF
067500011113     C*
067600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
067700011113     C                   Z-ADD     vabIAS        wrkDec
067800011113     C                   IF        wrkDec > *zeros
067900011113     C                   IF        vabVAS = 'ITL'
068000011113     C                   EVAL      vabIAS = *zeros
068100011113     C                   ENDIF
068200011113     C                   ENDIF
068300011113     C*
068400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
068500011113     C                   Z-ADD     vabCAS        wrkDec
068600011113     C                   IF        wrkDec > *zeros
068700011113     C                   IF        vabVCA = 'ITL'
068800011113     C                   EVAL      vabCAS = *zeros
068900011113     C                   ENDIF
069000011113     C                   ENDIF
069100011113     C*
069200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
069300011113     C                   Z-ADD     vabVMD        wrkDec
069400011113     C                   IF        wrkDec > *zeros
069500011113     C                   IF        vabVAD = 'ITL'
069600011113     C                   EVAL      vabVMD = *zeros
069700011113     C                   ENDIF
069800011113     C                   ENDIF
069900011113     C*
070000011113     C                   ENDSR
070100011113     C***
070200011113
070300011113
070400000801
070500000801
070600990920      /TITLE Invio dei dati al punto operativo.
070700010202     C     invio         BEGSR
070800990920     C*
070900021113     C* 1° invio FIVAT
071000010201     C                   reset                   dscmz
071100010201     C                   move      vlrpoi        cmzdst
071200021113     C                   eval      cmzfld = 'FIVATWWR'
071300010201     C                   eval      cmzmbd = vlrhdl
071400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
071500021009     C***                if        prmfir = *blanks
071600021113     C                   eval      cmzfla = 'FIVAT00F'
071700021113     C                   eval      cmzmba = 'FIVAT00F'
071800021009     C***                else
071900021009     C***                eval      cmzfla = prmfir
072000021009     C***                eval      cmzmba = prmfir
072100021009     C***                endif
072200010201     C                   eval      cmznrr = *zeros
072300020305     C                   move      §ctrokvt      cmznrr
072400021018     C                   eval      cmzlba = vlrfl1
072500010201     C                   call(e)   'TIS711C'
072600010201     C                   parm                    dscmz
072700010201     C                   parm      *blanks       esito
072800010205     C                   if        %error
072900010205     C                             or cmzerr = '1'
073000010205     C                             or esito  = '1'
073100010205     C                   eval      wrkesito = '3'
073200010205     C                   else
073300010201     C*
073400021113     C* 2° invio FIVAB
073500010201     C                   reset                   dscmz
073600010201     C                   move      vlrpoi        cmzdst
073700010201     C                   eval      cmzfld = vlrfou
073800010201     C                   eval      cmzmbd = vlrhdl
073900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
074000021009     C***                if        prmfir = *blanks
074100021113     C                   eval      cmzfla = 'FIVAB00F'
074200021113     C                   eval      cmzmba = 'FIVAB00F'
074300021009     C***                else
074400021009     C***                eval      cmzfla = prmfir
074500021009     C***                eval      cmzmba = prmfir
074600021009     C***                endif
074700010201     C                   eval      cmznrr = *zeros
074800010201     C                   move      §ctrokvb      cmznrr
074900021018     C                   eval      cmzlba = vlrfl1
075000010201     C                   call(e)   'TIS711C'
075100010201     C                   parm                    dscmz
075200010201     C                   parm      *blanks       esito
075300010201     C                   if        %error
075400010201     C                             or cmzerr = '1'
075500010201     C                             or esito  = '1'
075600010201     C                   eval      wrkesito = '3'
075700010201     C                   endif
075800010205     C                   endif
075900990920     C*
076000000613     C                   ENDSR
076100000613     C***
076200070411
076300070411     C     *pssr         BEGSR
076400070411     C*
076500070411     C                   if        %open(tivin00r)
076600070411     C                   close     tivin00r
076700070411     C                   endif
076800070411     C                   if        %open(fivabwwr)
076900070411     C                   close     fivabwwr
077000070411     C                   endif
077100070411     C                   if        %open(fivatwwr)
077200070411     C                   close     fivatwwr
077300070411     C                   endif
077400070411     C*
077500070411     C* Effettuo la chiamata al CLLE preposto
077600070411     C                   call(e)   'TITVVTC'
077700070411     C                   parm                    parccm
077800070411     C                   parm                    parmbr
077900070411     C                   parm      '2'           paropz
078000070411     C*
078100070411     C                   eval      wrkesito = '2'
078200070411     C*
078300070411     C                   seton                                        LR
078400070411     C*
078500070411     C                   ENDSR     '*CANCL'
078600070411     C***
078700070411
078800990910
078900000613     C     *inzsr        BEGSR
079000990910     C*
079100990910     C     *entry        plist
079200990920     C                   parm                    tivlrds
079300990921     C                   parm      wrkesito      esito
079400000724     C                   parm                    prmlit
079500000710     C                   parm                    prmfir
079600000613     C*
079700000830     C* CALCOLA LA DATA CORRENTE
079800091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
079900091223     C                   eval      datcor = %dec(%date() : *ISO)
080000000830     C*
080100000613     C                   ENDSR
080200000613     C***
080300990908
080400100412     Otitv1w5p  E            riepilogo         2
080500990915     O                                              'Upload via Internet'
080600990915     O                                           +1 'Traduzione TIVIN00R -'
080700021113     O                                           +1 'FIVABWWR/FIVATWWR'
080800010201     O                                           +1 'Report testate bolle'
080900990915     O          E            riepilogo   2
081000990915     O                       wrkdata
081100990915     O                       wrkora              +1
081200990915     O                       procname            +1
081300990915     O          E            riepilogo   2
081400990915     O                                              'Cliente..................:'
081500990915     O                       VABCCM        z     +1
081600990915     O          E            riepilogo   2
081700990920     O                                              'Riferimento Strategi.....:'
081800990920     O                       vlrhdl              +1
081900990915     O          E            riepilogo   2
082000990915     O                                              'Giusti...................:'
082100010201     O                       §CTROKVB      2   +  1
082200990915     O          E            riepilogo   2
082300990915     O                                              'Sbagliati e corretti.....:'
082400971022     O                       §CTRMO        2   +  1
082500990915     O          E            riepilogo   2
082600990915     O                                              'Sbagliati e scartati.....:'
082700971022     O                       §CTRNO        2   +  1
082800000613
082900100412     Otitv1w5ps E            testdett          2
083000000613     O                                              'Upload via Internet'
083100000613     O                                           +1 'Traduzione TIVIN00R -'
083200021113     O                                           +1 'FIVABWWR/FIVATWWR'
083300010201     O                                           +1 'Report testate bolle'
083400000616     O          E            testdett    3
083500000613     O                                           +2 'N° rec'
083600000613     O                                           +3 'Anteprima contenuto'
083700000616     O          E            rigadett    2
083800000613     O                       rrnum               +2
083900000621     O                       recko               +3
084000000616     O          E            findett     2
084100000613     O                       wrkdata
084200000613     O                       wrkora              +1
084300000613     O                       procname            +1
084400000616     O          E            findett     2
084500000613     O                                              'Cliente..................:'
084600000613     O                       VABCCM        z     +1
084700000616     O          E            findett     2
084800000613     O                                              'Riferimento Strategi.....:'
084900000613     O                       vlrhdl              +1
085000000616     O          E            findett     2
085100000613     O                                              'Giusti...................:'
085200010201     O                       §CTROKVB      2   +  1
085300000616     O          E            findett     2
085400000613     O                                              'Sbagliati e corretti.....:'
085500000613     O                       §CTRMO        2   +  1
085600000616     O          E            findett     2
085700000613     O                                              'Sbagliati e scartati.....:'
085800000613     O                       §CTRNO        2   +  1
085900000512** CMD - COMANDI CL
086000100412OVRPRTF FILE(TITV1W5P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
086100100412OVRPRTF FILE(TITV1W5PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
086200100412DLTOVR FILE(TITV1W5P TITV1W5PS) LVL(*)
086300000512
086400000512
