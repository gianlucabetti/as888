000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800100127     Ftitv1u6p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100100127     Ftitv1u6ps O    f  198        PRINTER usropn
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
003600100127     D depspe          s             80    INZ(*blanks)
003700100127     D curspe          s             80    INZ(*blanks)
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
005800091223     D trul33ds      e ds                  inz
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
011600100127     C                   if        not %open(titv1u6ps)
011700100127     C                   open      titv1u6ps
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
013600100127     C                   if        %open(titv1u6ps)
013700000616     C                   except    findett
013800100127     C                   close     titv1u6ps
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
015000000613     C*--------------------------------------------------------
015100000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015200000613     C*--------------------------------------------------------
015300000621     C     STPR          BEGSR
015400000613     C*
015500100127     C                   if        not %open(titv1u6p)
015600100127     C                   open      titv1u6p
015700990915     C                   endif
015800990915     C*
015900990915     C                   except    riepilogo
016000990915     C*
016100100127     C                   if        %open(titv1u6p)
016200100127     C                   close     titv1u6p
016300990914     C                   endif
016400990910     C*
016500000613     C                   ENDSR
016600000613     C***
016700990908
016800000801
016900910830     C*--------------------------------------------------------
017000070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
017100910830     C*--------------------------------------------------------
017200070530     C     RWFILE        BEGSR
017300990910     C*
017400990914     C                   if        not %open(tivin00r)
017500990908     C                   open      tivin00r
017600990914     C                   endif
017700021113     C                   if        not %open(fivabwwr)
017800021113     C                   open      fivabwwr
017900990914     C                   endif
018000070530     C*
018100021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
018200020305     C                   exsr      prevat
018300010201     C*
018400010202     C                   if        chkcall = '0'
018500010202     C*
018600021113     C                   if        not %open(fivatwwr)
018700021113     C                   open      fivatwwr
018800010201     C                   endif
018900080117     C*
019000080117     C                   EXSR      INZVAR
019100080117     C                   EXSR      DEFCAM
019200990910     C*
019300010201     C                   clear                   §CTROKVB          5 0
019400020305     C                   clear                   §CTROKVT          5 0
019500000801     C                   clear                   §CTRMO            5 0
019600000801     C                   clear                   §CTRNO            5 0
019700990910     C*
019800020725     C/EXEC SQL
019900020725     C+ declare C1 cursor for select
020000100127     C+ vindta, vinflg, substr(vindta, 257, 80) as sped, rrn(tivin00r)
020100060223     C+ from tivin00r
020200060223     C+ order by sped
020300020725     C+ for read only
020400020725     C/END-EXEC
020500020725     C
020600020725     C/EXEC SQL
020700020725     C+ open C1
020800020725     C/END-EXEC
020900020725     C
021000020725     C/EXEC SQL
021100070530     C+ Fetch C1 into :INPUT_DS
021200020725     C/END-EXEC
021300020725     C*
021400020725     C                   dow       sqlcod = *zeros
021500990913     C*
021600020725     C                   if        vindta > *blanks
021700000613     C                   add       1             rrnum
021800000801     C*
021900020725     C                   if        vinflg = *blanks
022000020725     C                             or vinflg = '0'
022100020725     C                             or vinflg = '2'
022200000801     C*
022300020725     C                   clear                   x_vinmsg
022400020725     C                   eval      x_vinflg = '1'
022500010305     C*
022600010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
022700100127     C                   EVAL      curspe=%trim(%subst(vindta:257:80))
022800010305     C*
022900071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
023000080923     C                   if        *in50 = *off
023100091223     C                   exsr      repNSP
023200071003     C                   exsr      impvab
023300071003     C                   exsr      wrivab
023400100127     C                   exsr      wrivat_a                                     => carico VAT
023500071003     C                   exsr      wrivat_b                                     => carico VAT
023600071003     C                   else
023700080923     C*
023800071009     C                   if        wDISK = 'D'
023900091223     C                   exsr      repNSP
024000071009     C                   exsr      impvab
024100071009     C                   exsr      wrivab
024200100127     C                   exsr      wrivat_a                                     => carico VAT
024300071009     C                   exsr      wrivat_b                                     => carico VAT
024400071009     C                   exsr      wrivat_e                                     => carico VAT
024500071009     C                   else
024600071009     C*
024700010305     C                   if        depspe = *blanks                             => 1° giro
024800010305     C                   eval      depspe = curspe                              => memorizz. spediz
024900080117     C                   clear                   tivinds
025000091223     C                   exsr      repNSP
025100020305     C                   exsr      impvab
025200100127     C                   exsr      wrivat_a                                     => carico VAT
025300071003     C                   exsr      wrivat_b                                     => carico VAT
025400071003     C   50              exsr      wrivat_e                                     => carico VAT
025500010305     C                   else
025600020725     C                   if        depspe <> curspe                             => rottura di spediz
025700010305     C                   eval      depspe = curspe                              => memorizz. spediz
025800070928     C                   exsr      wrivab
025900080117     C                   clear                   tivinds
026000091223     C                   exsr      repNSP
026100020305     C                   exsr      impvab
026200100127     C                   exsr      wrivat_a                                     => carico VAT
026300071003     C                   exsr      wrivat_b                                     => carico VAT
026400071003     C   50              exsr      wrivat_e                                     => carico VAT
026500020305     C                   else                                                   => x stessa spediz
026600020305     C                   exsr      impvab
026700100127     C                   exsr      wrivat_a                                     => carico VAT
026800071003     C                   exsr      wrivat_b                                     => carico VAT
026900071003     C   50              exsr      wrivat_e                                     => carico VAT
027000010305     C                   endif
027100010305     C                   endif
027200010305     C                   endif
027300071003     C                   endif
027400071009     C                   endif
027500000905     C*
027600000905     C                   else
027700020725     C                   eval      x_vinflg = '1'
027800000905     C                   endif
027900000905     C*
028000020725     C     VINRRN        chain     tivin000
028100020725     C                   if        %found(tivin00r)
028200020725     C                   eval      y_vinflg = x_vinflg
028300020725     C                   eval      y_vinmsg = x_vinmsg
028400020725     C                   update    tivin000
028500020725     C                   endif
028600020725     C*
028700020725     C/EXEC SQL
028800070530     C+ Fetch C1 into :INPUT_DS
028900020725     C/END-EXEC
029000020725     C*
029100020725     C                   enddo
029200020725     C*
029300020725     C/EXEC SQL
029400020725     C+ close C1
029500020725     C/END-EXEC
029600000905     C*
029700020305     C* Scarico i VAB rimasti "in sospeso"
029800071009     C                   if        wDISK = 'C'
029900071009     C                   exsr      wrivab
030000071009     C                   endif
030100010202     C*
030200010202     C                   endif
030300990910
030400990910     C* Se non ci sono record con errori ...
030500000710     C                   if        §ctrno = 0
030600990910     C* ... restituisco esito OK.
030700990921     C                   eval      wrkesito = '0'
030800990910     C                   else
030900010201     C                   if        §ctrokvb > 0
031000990921     C                   eval      wrkesito = '1'
031100000710     C                   else
031200000710     C                   eval      wrkesito = '2'
031300990910     C                   endif
031400000710     C                   endif
031500990910     C*
031600990914     C                   if        %open(tivin00r)
031700990908     C                   close     tivin00r
031800990914     C                   endif
031900021113     C                   if        %open(fivabwwr)
032000021113     C                   close     fivabwwr
032100990914     C                   endif
032200021113     C                   if        %open(fivatwwr)
032300021113     C                   close     fivatwwr
032400010201     C                   endif
032500990910     C*
032600010201     C                   if        §ctrokvb > 0
032700000724     C                             and vlrpoi <> *zeros
032800010202     C                   exsr      invio
032900990920     C                   endif
033000990920     C*
033100910830     C                   ENDSR
033200000613     C***
033300010305
033400010305     C*----------------------------------------------------*
033500020305     C*  SCARICAMENTO BUFFER RECORDS VAB
033600010305     C*----------------------------------------------------*
033700020305     C     WRIVAB        BEGSR
033800070730     C*
033900071003     C* Considerazioni finali
034000071003     C                   if        VABRMA = *blanks
034100071003     C                   movel(P)  VABRMN        VABRMA
034200071003     C                   endif
034300071003     C*
034400021113     C                   write     fivab000                                     => scarico il VAB
034500010305     C*
034600010305     C                   ENDSR
034700990920
034800000801     C*----------------------------------------------------*
034900000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
035000000801     C*----------------------------------------------------*
035100010201     C     INZVAR        BEGSR
035200000801     C*
035300010201     C                   Z-ADD     *zeros        Num5_0
035400020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
035500020725     C                   MOVEL     '0'           FlgCAS            1
035600000801     C*
035700000801     C                   ENDSR
035800000801     C*----------------------------------------------------*
035900000801     C*  IMPOSTAZIONE CAMPI COSTANTI
036000000801     C*----------------------------------------------------*
036100020904     C     DEFCAM        BEGSR
036200080923     C*
036300090310     C                   SETOFF                                       50
036400000801     C*
036500021113     C                   CLEAR                   FIVAB000
036600021113     C                   CLEAR                   FIVAT000
036700070730     C* Imposto i valori di default...
036800100127     C                   EVAL      VABCCM = 0990793
036900100127     C                   EVAL      VATCCM = 0990793
037000100122     C                   EVAL      VABLNP = 099
037100100122     C                   EVAL      VATLNP = 099
037200100127     C                   EVAL      VABCTR = 000
037300070730     C                   EVAL      VABCBO = '1'
037400100122     C                   EVAL      VABCTM = *blanks
037500070222     C* ... e poi verifico se sono stati passati come parametri
037600070222     C                   IF        vlrppt > *blanks
037700070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
037800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
037900070222     C                   EXSR      CHKNUM
038000070222     C                   IF        PiInt=*on
038100070222     C                   Z-ADD     PiVal         VABCCM
038200070222     C                   Z-ADD     PiVal         VATCCM
038300070222     C                   ENDIF
038400070222     C                   ENDIF
038500070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
038600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
038700070222     C                   EXSR      CHKNUM
038800070222     C                   IF        PiInt=*on
038900070222     C                   Z-ADD     PiVal         VABLNP
039000070222     C                   Z-ADD     PiVal         VATLNP
039100070222     C                   ENDIF
039200070222     C                   ENDIF
039300070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
039400070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
039500070222     C                   EXSR      CHKNUM
039600070222     C                   IF        PiInt=*on
039700070222     C                   Z-ADD     PiVal         VABCTR
039800070222     C                   ENDIF
039900070928     C                   ENDIF
040000071009     C                   MOVEL     *blanks       wDISK             1
040100071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
040200071009     C                   IF        wDISK <> *blanks
040300070928     C                   SETON                                        50
040400070222     C                   ENDIF
040500070222     C                   ENDIF
040600071009     C*
040700071009     C   50              EVAL      VABCTM = '7Q'
040800000801     C*
040900000801     C                   ENDSR
041000091223     C*----------------------------------------------------*
041100091223     C*  STACCO NUMERATORE X NUMERO SPEDIZIONE
041200091223     C*----------------------------------------------------*
041300091223     C     REPNSP        BEGSR
041400091223     C*
041500091223     C                   EXSR      INZVAR
041600091223     C                   EXSR      DEFCAM
041700091223     C*
041800091223     C* NSP => Stacco un numeratore da AZNUM
041900091223     C                   clear                   TRUL33DS
042000091223     C                   eval      I33OPE = *zeros
042100091223     C                   eval      I33CNU = 302
042200091223     C                   eval      I33NUM = 1
042300091223     C                   movel     TRUL33DS      KPJBU
042400091223     C                   call      'TRUL33R'
042500091223     C                   parm                    KPJBA
042600091223     C                   movel     KPJBU         TRUL33DS
042700091223     C                   if        O33ERR = *zeros
042800091223     C                   z-add     O33NRF        VABNSP
042900091223     C                   z-add     O33NRF        VATNSP
043000091223     C                   else
043100091223     C                   SETON                                        31
043200091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043300091223     C                             + ' ' + 'VABNSP VATNSP'
043400091223     C                   endif
043500091223     C*
043600091223     C                   ENDSR
043700000801     C*----------------------------------------------------*
043800021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
043900000801     C*----------------------------------------------------*
044000010201     C     IMPVAB        BEGSR
044100070530     C*
044200070530     C                   SETOFF                                       3132
044300070928     C*
044400070928     C                   MOVE(P)   vlrpoi        VABFGS
044500070928     C                   MOVE(P)   vlrpoi        VATFGS
044600070928     C*
044700070928     C                   MOVEL     datcor        VABAAS
044800070928     C                   MOVEL     datcor        VATAAS
044900070928     C                   MOVE      datcor        VABMGS
045000100127     C                   EVAL      VABRSD=%trim(%subst(vindta:1:35))
045100070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
045200070928     C     '@':'A'       XLATE     VABRSD        VABRSD
045300070928     C* ==
045400100127     C                   EVAL      VABIND=%trim(%subst(vindta:36:35))
045500100127     C                   EVAL      VABLOD=%trim(%subst(vindta:71:30))
045600100127     C                   EVAL      VABPRD=%trim(%subst(vindta:106:2))
045700100127     C                   EVAL      VABRMA=%trim(%subst(vindta:257:80))
045800100127     C                   EVAL      VABNOT=%trim(%subst(vindta:147:60))
045900100127     C                   EVAL      VABNT2=%trim(%subst(vindta:147+35:60-35))
046000100127     C                   IF        %subst(vindta:207:1) = 'F'
046100100127     C                   EVAL      VABCBO='1'
046200100127     C                   ENDIF
046300100127     C                   IF        %subst(vindta:207:1) = 'A'
046400100127     C                   EVAL      VABCBO='2'
046500100127     C                   ENDIF
046600100127     C                   IF        %subst(vindta:208:1) = 'S'
046700100127     C                   EVAL      VABFFD='S'
046800100127     C                   ENDIF
046900071003     C* CAD
047000100127     C                   EVAL      PiStr=%trim(%subst(vindta:101:5))
047100070928     C                   EXSR      CHKNUM
047200070928     C                   IF        PiInt=*on
047300070928     C                   Z-ADD     PiVal         Num5_0
047400070928     C                   MOVEL     Num5_0        VABCAD
047500070928     C                   ELSE
047600070928     C                   SETON                                        32
047700070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047800070928     C                             + ' ' + 'VABCAD'
047900070928     C                   ENDIF
048000080923     C* RMN
048100100127     C                   EVAL      PiStr=%trim(%subst(vindta:257:80))
048200080923     C                   EXSR      CHKNUM
048300080923     C                   IF        PiInt=*on
048400080923     C                   Z-ADD     PiVal         VABRMN
048500080923     C                   ELSE
048600080923     C                   SETON                                        32
048700080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048800080923     C                             + ' ' + 'VABRMN'
048900080923     C                   ENDIF
049000091223     C* NCL
049100100127     C                   EVAL      PiStr=%trim(%subst(vindta:124:5))
049200070928     C                   EXSR      CHKNUM
049300070928     C                   IF        PiInt=*on
049400091223     C                   ADD       PiVal         VABNCL
049500070928     C                   ELSE
049600070928     C                   SETON                                        32
049700070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049800070928     C                             + ' ' + 'VABNCL'
049900070928     C                   ENDIF
050000100122     C* PKB
050100100127     C                   IF        %trim(%subst(vindta:131:6))<>*blanks
050200100122     C                             AND
050300100127     C                             %trim(%subst(vindta:131:6))<>*zeros
050400100127     C                             AND
050500100127     C                             %trim(%subst(vindta:131:6))<>'0,00'
050600100127     C                   EVAL      PiStr=%trim(%subst(vindta:131:6))
050700070928     C                   EXSR      CHKNUM
050800070928     C                   IF        PiNum=*on
050900091223     C                   ADD       PiVal         VABPKB
051000070928     C                   ELSE
051100070928     C                   SETON                                        32
051200070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051300070928     C                             + ' ' + 'VABPKB'
051400100122     C                   ENDIF
051500070928     C                   ENDIF
051600100127     C* VLB
051700100127     C                   IF        %trim(%subst(vindta:234:11))<>*blanks
051800091223     C                             AND
051900100127     C                             %trim(%subst(vindta:234:11))<>*zeros
052000100127     C                             AND
052100100127     C                             %trim(%subst(vindta:234:11))<>'0,0'
052200100127     C                   EVAL      PiStr=%trim(%subst(vindta:234:11))
052300091223     C                   EXSR      CHKNUM
052400091223     C                   IF        PiNum=*on
052500091223     C                   ADD       PiVal         VABVLB
052600091223     C                   ELSE
052700091223     C                   SETON                                        32
052800091223     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052900091223     C                             + ' ' + 'VABVLB'
053000091223     C                   ENDIF
053100091223     C                   ENDIF
053200080923     C* CAS
053300100127     C                   IF        %trim(%subst(vindta:137:10))<>*blanks
053400080923     C                             AND
053500100127     C                             %trim(%subst(vindta:137:10))<>*zeros
053600080923     C                             AND
053700100127     C                             %trim(%subst(vindta:137:10))<>'0,00'
053800070928     C                   MOVEL     '1'           FlgCAS
053900100127     C                   EVAL      VABVCA='EUR'
054000100127     C                   EVAL      PiStr=%trim(%subst(vindta:137:10))
054100070928     C                   EXSR      CHKNUM
054200070928     C                   IF        PiNum=*on
054300070928     C                   Z-ADD     PiVal         VABCAS
054400070928     C                   ELSE
054500070928     C                   SETON                                        32
054600070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054700070928     C                             + ' ' + 'VABCAS'
054800070928     C                   ENDIF
054900070928     C                   ENDIF
055000100127     C* IAS
055100100127     C                   IF        %trim(%subst(vindta:223:11))<>*blanks
055200100127     C                             AND
055300100127     C                             %trim(%subst(vindta:223:11))<>*zeros
055400100127     C                             AND
055500100127     C                             %trim(%subst(vindta:223:11))<>'0,00'
055600100127     C                   EVAL      PiStr=%trim(%subst(vindta:223:11))
055700100127     C                   EXSR      CHKNUM
055800100127     C                   IF        PiNum=*on
055900100127     C                   Z-ADD     PiVal         VABIAS
056000100127     C                   ELSE
056100100127     C                   SETON                                        32
056200100127     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056300100127     C                             + ' ' + 'VABIAS'
056400100127     C                   ENDIF
056500100127     C                   ENDIF
056600070730     C*
056700070730     C* Considerazioni finali su CBO/CAS
056800070730     C                   IF        FlgCAS = '1'
056900070730     C                   IF        VABCBO = '1'
057000070730     C                   EVAL      VABCBO = '4'
057100070730     C                   ENDIF
057200070730     C                   IF        VABCBO = '2'
057300070730     C                   EVAL      VABCBO = '6'
057400070730     C                   ENDIF
057500070730     C                   ENDIF
057600020305     C*
057700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
057800011113     C                   EXSR      CHKIMPDIV
057900010202     C*
058000000801     C* Ebbene...
058100000801     C                   ADD       1             §CTRMO
058200070530     C                   IF        *in31 <> *zeros OR
058300070530     C                             *in32 <> *zeros
058400000801     C                   ADD       1             §CTRNO
058500000801     C                   EVAL      recko = vindta
058600000801     C                   EXCEPT    rigadett
058700020725     C                   EVAL      x_vinflg = '2'
058800000801     C                   ELSE
058900010201     C                   ADD       1             §CTROKVB
059000000801     C                   ENDIF
059100000801     C*
059200000801     C                   ENDSR
059300100127     C*----------------------------------------------------*
059400100127     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "A"
059500100127     C*----------------------------------------------------*
059600100127     C     WRIVAT_A      BEGSR
059700100127     C*
059800100127     C* Valorizzo l buffer di scrittura del FIVAT
059900100127     C* - TIPO RECORD "A"
060000100127     C***                if        %subst(vindta:92:15) <> *blanks
060100100127     C***                eval      VATTRC = 'A'
060200100127     C***                eval      VATNOT = %trim(%subst(vindta:92:15))
060300100127     C***                exsr      wrivat
060400100127     C***                endif
060500100127     C*
060600100127     C                   ENDSR
060700010201     C*----------------------------------------------------*
060800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
060900010201     C*----------------------------------------------------*
061000071003     C     WRIVAT_B      BEGSR
061100010201     C*
061200021113     C* Valorizzo l buffer di scrittura del FIVAT
061300070928     C* - TIPO RECORD "B"
061400100127     C***                if        %subst(vindta:92:15) <> *blanks
061500100127     C***                eval      VATTRC = 'B'
061600100127     C***                eval      VATNOT = %trim(%subst(vindta:92:15))
061700100127     C***                exsr      wrivat
061800100127     C***                endif
061900010201     C*
062000010201     C                   ENDSR
062100071003     C*----------------------------------------------------*
062200071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
062300071003     C*----------------------------------------------------*
062400071003     C     WRIVAT_E      BEGSR
062500071003     C*
062600071003     C* Valorizzo l buffer di scrittura del FIVAT
062700071003     C* - TIPO RECORD "E"
062800100127     C***                if        %subst(vindta:331:26) <> *blanks
062900100127     C***                eval      VATTRC = 'E'
063000100127     C***                eval      VATNOT = %trim(%subst(vindta:331:26))
063100100127     C***                exsr      wrivat
063200100127     C***                endif
063300071003     C*
063400071003     C                   ENDSR
063500100127     C*----------------------------------------------------*
063600100127     C*  SCARICO BUFFER FIVAT
063700100127     C*----------------------------------------------------*
063800100127     C     WRIVAT        BEGSR
063900100127     C*
064000100127     C                   if        vatNOT <> *blanks
064100100127     C                   ADD       1             §CTROKVT
064200100127     C                   write     FIVAT000
064300100127     C                   endif
064400100127     C*
064500100127     C                   ENDSR
064600020904
064700020904
064800010202     C*----------------------------------------------------*
064900021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
065000010202     C*----------------------------------------------------*
065100020305     C     PREVAT        BEGSR
065200010202     C*
065300021113     C* Compongo il nome del membro da dare al FIVATWWR
065400010202     C                   eval      parmbr = vlrhdl
065500010202     C                   movel     'M'           parmbr
065600070530     C                   eval      parccm = %subst(vlrKSC:2:7)
065700010202     C                   eval      paropz = '1'
065800010202     C* Effettuo la chiamata al CLLE preposto
065900050128     C                   call(e)   'TITVVTC'
066000010202     C                   parm                    parccm
066100010202     C                   parm                    parmbr
066200010202     C                   parm                    paropz
066300010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
066400010202     C                   if        %error
066500010202     C                   movel     '1'           chkcall
066600010202     C                   else
066700010202     C                   movel     '0'           chkcall
066800010202     C                   endif
066900010202     C*
067000010202     C                   ENDSR
067100000801     C*----------------------------------------------------*
067200000801     C*  CONTROLLO NUMERICITA' CAMPI
067300000801     C*----------------------------------------------------*
067400000801     C     CHKNUM        BEGSR
067500081113     C*
067600081113     C                   IF        PiDecChr = *blanks
067700091223     C                   EVAL      PiDecChr = ','
067800081113     C                   ENDIF
067900091223     C*
068000081113     C                   callp(e)  UBISNUM_Check(PiStr
068100081113     C                                          :PiDecChr
068200081113     C                                          :PiVal
068300081113     C                                          :PiNum
068400081113     C                                          :PiInt)
068500081113     C*
068600000801     C                   IF        %error
068700000801     C                   EVAL      PiInt=*off
068800000801     C                   ENDIF
068900000801     C*
069000000801     C                   ENDSR
069100000801     C***
069200000801
069300011113
069400011113     C*----------------------------------------------------*
069500011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
069600011113     C*----------------------------------------------------*
069700011113     C     CHKIMPDIV     BEGSR
069800011113     C*
069900011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
070000011113     C                   Z-ADD     *zeros        wrkDec            9 9
070100011113     C*
070200011113     C* Come prima cosa effettuo considerazioni sulla divisa
070300011113     C                   IF        vabIAS > *zeros
070400011113     C                   IF        vabVAS <> 'EUR'
070500011113     C                   EVAL      vabVAS =  'ITL'
070600011113     C                   ENDIF
070700011113     C                   ENDIF
070800011113     C*
070900011113     C                   IF        vabCAS > *zeros
071000011113     C                   IF        vabVCA <> 'EUR'
071100011113     C                   EVAL      vabVCA =  'ITL'
071200011113     C                   ENDIF
071300011113     C                   ENDIF
071400011113     C*
071500011113     C                   IF        vabVMD > *zeros
071600020305     C                   IF        vabVAD <> 'EUR'
071700011113     C                   EVAL      vabVAD =  'ITL'
071800011113     C                   ENDIF
071900011113     C                   ENDIF
072000011113     C*
072100011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
072200011113     C                   Z-ADD     vabIAS        wrkDec
072300011113     C                   IF        wrkDec > *zeros
072400011113     C                   IF        vabVAS = 'ITL'
072500011113     C                   EVAL      vabIAS = *zeros
072600011113     C                   ENDIF
072700011113     C                   ENDIF
072800011113     C*
072900011113     C* Stabilisco se il contrasegno ha decimali valorizzati
073000011113     C                   Z-ADD     vabCAS        wrkDec
073100011113     C                   IF        wrkDec > *zeros
073200011113     C                   IF        vabVCA = 'ITL'
073300011113     C                   EVAL      vabCAS = *zeros
073400011113     C                   ENDIF
073500011113     C                   ENDIF
073600011113     C*
073700011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
073800011113     C                   Z-ADD     vabVMD        wrkDec
073900011113     C                   IF        wrkDec > *zeros
074000011113     C                   IF        vabVAD = 'ITL'
074100011113     C                   EVAL      vabVMD = *zeros
074200011113     C                   ENDIF
074300011113     C                   ENDIF
074400011113     C*
074500011113     C                   ENDSR
074600011113     C***
074700011113
074800011113
074900000801
075000000801
075100990920      /TITLE Invio dei dati al punto operativo.
075200010202     C     invio         BEGSR
075300990920     C*
075400021113     C* 1° invio FIVAT
075500010201     C                   reset                   dscmz
075600010201     C                   move      vlrpoi        cmzdst
075700021113     C                   eval      cmzfld = 'FIVATWWR'
075800010201     C                   eval      cmzmbd = vlrhdl
075900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
076000021009     C***                if        prmfir = *blanks
076100021113     C                   eval      cmzfla = 'FIVAT00F'
076200021113     C                   eval      cmzmba = 'FIVAT00F'
076300021009     C***                else
076400021009     C***                eval      cmzfla = prmfir
076500021009     C***                eval      cmzmba = prmfir
076600021009     C***                endif
076700010201     C                   eval      cmznrr = *zeros
076800020305     C                   move      §ctrokvt      cmznrr
076900021018     C                   eval      cmzlba = vlrfl1
077000010201     C                   call(e)   'TIS711C'
077100010201     C                   parm                    dscmz
077200010201     C                   parm      *blanks       esito
077300010205     C                   if        %error
077400010205     C                             or cmzerr = '1'
077500010205     C                             or esito  = '1'
077600010205     C                   eval      wrkesito = '3'
077700010205     C                   else
077800010201     C*
077900021113     C* 2° invio FIVAB
078000010201     C                   reset                   dscmz
078100010201     C                   move      vlrpoi        cmzdst
078200010201     C                   eval      cmzfld = vlrfou
078300010201     C                   eval      cmzmbd = vlrhdl
078400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078500021009     C***                if        prmfir = *blanks
078600021113     C                   eval      cmzfla = 'FIVAB00F'
078700021113     C                   eval      cmzmba = 'FIVAB00F'
078800021009     C***                else
078900021009     C***                eval      cmzfla = prmfir
079000021009     C***                eval      cmzmba = prmfir
079100021009     C***                endif
079200010201     C                   eval      cmznrr = *zeros
079300010201     C                   move      §ctrokvb      cmznrr
079400021018     C                   eval      cmzlba = vlrfl1
079500010201     C                   call(e)   'TIS711C'
079600010201     C                   parm                    dscmz
079700010201     C                   parm      *blanks       esito
079800010201     C                   if        %error
079900010201     C                             or cmzerr = '1'
080000010201     C                             or esito  = '1'
080100010201     C                   eval      wrkesito = '3'
080200010201     C                   endif
080300010205     C                   endif
080400990920     C*
080500000613     C                   ENDSR
080600000613     C***
080700070411
080800070411     C     *pssr         BEGSR
080900070411     C*
081000070411     C                   if        %open(tivin00r)
081100070411     C                   close     tivin00r
081200070411     C                   endif
081300070411     C                   if        %open(fivabwwr)
081400070411     C                   close     fivabwwr
081500070411     C                   endif
081600070411     C                   if        %open(fivatwwr)
081700070411     C                   close     fivatwwr
081800070411     C                   endif
081900070411     C*
082000070411     C* Effettuo la chiamata al CLLE preposto
082100070411     C                   call(e)   'TITVVTC'
082200070411     C                   parm                    parccm
082300070411     C                   parm                    parmbr
082400070411     C                   parm      '2'           paropz
082500070411     C*
082600070411     C                   eval      wrkesito = '2'
082700070411     C*
082800070411     C                   seton                                        LR
082900070411     C*
083000070411     C                   ENDSR     '*CANCL'
083100070411     C***
083200070411
083300990910
083400000613     C     *inzsr        BEGSR
083500990910     C*
083600990910     C     *entry        plist
083700990920     C                   parm                    tivlrds
083800990921     C                   parm      wrkesito      esito
083900000724     C                   parm                    prmlit
084000000710     C                   parm                    prmfir
084100000613     C*
084200000830     C* CALCOLA LA DATA CORRENTE
084300091223     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
084400091223     C                   eval      datcor = %dec(%date() : *ISO)
084500000830     C*
084600000613     C                   ENDSR
084700000613     C***
084800990908
084900100127     Otitv1u6p  E            riepilogo         2
085000990915     O                                              'Upload via Internet'
085100990915     O                                           +1 'Traduzione TIVIN00R -'
085200021113     O                                           +1 'FIVABWWR/FIVATWWR'
085300010201     O                                           +1 'Report testate bolle'
085400990915     O          E            riepilogo   2
085500990915     O                       wrkdata
085600990915     O                       wrkora              +1
085700990915     O                       procname            +1
085800990915     O          E            riepilogo   2
085900990915     O                                              'Cliente..................:'
086000990915     O                       VABCCM        z     +1
086100990915     O          E            riepilogo   2
086200990920     O                                              'Riferimento Strategi.....:'
086300990920     O                       vlrhdl              +1
086400990915     O          E            riepilogo   2
086500990915     O                                              'Giusti...................:'
086600010201     O                       §CTROKVB      2   +  1
086700990915     O          E            riepilogo   2
086800990915     O                                              'Sbagliati e corretti.....:'
086900971022     O                       §CTRMO        2   +  1
087000990915     O          E            riepilogo   2
087100990915     O                                              'Sbagliati e scartati.....:'
087200971022     O                       §CTRNO        2   +  1
087300000613
087400100127     Otitv1u6ps E            testdett          2
087500000613     O                                              'Upload via Internet'
087600000613     O                                           +1 'Traduzione TIVIN00R -'
087700021113     O                                           +1 'FIVABWWR/FIVATWWR'
087800010201     O                                           +1 'Report testate bolle'
087900000616     O          E            testdett    3
088000000613     O                                           +2 'N° rec'
088100000613     O                                           +3 'Anteprima contenuto'
088200000616     O          E            rigadett    2
088300000613     O                       rrnum               +2
088400000621     O                       recko               +3
088500000616     O          E            findett     2
088600000613     O                       wrkdata
088700000613     O                       wrkora              +1
088800000613     O                       procname            +1
088900000616     O          E            findett     2
089000000613     O                                              'Cliente..................:'
089100000613     O                       VABCCM        z     +1
089200000616     O          E            findett     2
089300000613     O                                              'Riferimento Strategi.....:'
089400000613     O                       vlrhdl              +1
089500000616     O          E            findett     2
089600000613     O                                              'Giusti...................:'
089700010201     O                       §CTROKVB      2   +  1
089800000616     O          E            findett     2
089900000613     O                                              'Sbagliati e corretti.....:'
090000000613     O                       §CTRMO        2   +  1
090100000616     O          E            findett     2
090200000613     O                                              'Sbagliati e scartati.....:'
090300000613     O                       §CTRNO        2   +  1
090400000512** CMD - COMANDI CL
090500100127OVRPRTF FILE(TITV1U6P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
090600100127OVRPRTF FILE(TITV1U6PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
090700100127DLTOVR FILE(TITV1U6P TITV1U6PS) LVL(*)
090800000512
090900000512
