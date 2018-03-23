000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800110524     Ftitv2f8p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100110524     Ftitv2f8ps O    f  198        PRINTER usropn
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
003600110524     D depspe          s              8    INZ(*blanks)
003700110524     D curspe          s              8    INZ(*blanks)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004300070530     D Num5_0          s              5  0
004400000830
004500020725
004600020725     D*------------------
004700020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
004800020725     D*------------------
004900070530     D INPUT_DS        DS                  INZ
005000101007     D  VINDTA                     2048
005100101007     D  VINFLG                        1
005200110524     D  VINSPE                        8
005300101007     D  VINRRN                        8  0
005400020725     D*
005500080923     D*------------------
005600080923     D* DS REPERIMENTO NUMERATORE
005700080923     D*------------------
005800080923     D trul33ds      e ds                  inz
005900080923     D*------------------
006000080923     D* DS ARCHITETTURA
006100080923     D*------------------
006200080923     D kpjba         e ds                  inz
006300080923     D*------------------
006400081113
006500081113     D*------------------
006600081113     D* LINKING A DEFINIZIONI ESTERNE
006700081113     D*------------------
006800100324     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
006900081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
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
008800111031     C                   if        vlrpoi <> *zeros AND vlrpoi <> 999
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
011600110524     C                   if        not %open(titv2f8ps)
011700110524     C                   open      titv2f8ps
011800000616     C                   except    testdett
011900000613     C                   endif
012000000613     C*
012100070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
012200111031     C                   if        vlrpoi <> *zeros AND vlrpoi <> 999
012300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012400111031     C                   endif
012500000613     C*
012600010202     C* Effettuo la chiamata al CLLE preposto
012700050128     C                   call(e)   'TITVVTC'
012800010202     C                   parm                    parccm
012900010202     C                   parm                    parmbr
013000010202     C                   parm      '2'           paropz
013100070730     C*
013200070730     C* Effettuo lancio TISI95 solo x chiusura
013300070730     C                   CLEAR                   TISI95DS
013400070730     C                   EVAL      I95TLA = 'C'
013500070730     C                   CALL      'TISI95R'
013600070730     C                   PARM                    TISI95DS
013700010202     C*
013800110524     C                   if        %open(titv2f8ps)
013900000616     C                   except    findett
014000110524     C                   close     titv2f8ps
014100000613     C                   endif
014200000616     C*
014300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014400111031     C                   if        vlrpoi <> *zeros AND vlrpoi <> 999
014500011113     C                   Z-ADD     150           LENGH            15 5
014600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014700000616     C                   PARM                    CMD(3)
014800000616     C                   PARM                    LENGH
014900111031     C                   endif
015000000616     C*
015100000801     C
015200010201     C                   seton                                        LR
015300000613
015400000613     C*--------------------------------------------------------
015500000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015600000613     C*--------------------------------------------------------
015700000621     C     STPR          BEGSR
015800000613     C*
015900110524     C                   if        not %open(titv2f8p)
016000110524     C                   open      titv2f8p
016100990915     C                   endif
016200990915     C*
016300990915     C                   except    riepilogo
016400990915     C*
016500110524     C                   if        %open(titv2f8p)
016600110524     C                   close     titv2f8p
016700990914     C                   endif
016800990910     C*
016900000613     C                   ENDSR
017000000613     C***
017100990908
017200000801
017300910830     C*--------------------------------------------------------
017400070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
017500910830     C*--------------------------------------------------------
017600070530     C     RWFILE        BEGSR
017700990910     C*
017800990914     C                   if        not %open(tivin00r)
017900990908     C                   open      tivin00r
018000990914     C                   endif
018100021113     C                   if        not %open(fivabwwr)
018200021113     C                   open      fivabwwr
018300990914     C                   endif
018400070530     C*
018500021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
018600020305     C                   exsr      prevat
018700010201     C*
018800010202     C                   if        chkcall = '0'
018900010202     C*
019000021113     C                   if        not %open(fivatwwr)
019100021113     C                   open      fivatwwr
019200010201     C                   endif
019300080117     C*
019400080117     C                   EXSR      INZVAR
019500080117     C                   EXSR      DEFCAM
019600990910     C*
019700010201     C                   clear                   §CTROKVB          5 0
019800020305     C                   clear                   §CTROKVT          5 0
019900000801     C                   clear                   §CTRMO            5 0
020000000801     C                   clear                   §CTRNO            5 0
020100990910     C*
020200020725     C/EXEC SQL
020300020725     C+ declare C1 cursor for select
020400110524     C+ vindta, vinflg, substr(vindta, 322, 8) as sped, rrn(tivin00r)
020500060223     C+ from tivin00r
020600060223     C+ order by sped
020700020725     C+ for read only
020800020725     C/END-EXEC
020900020725     C
021000020725     C/EXEC SQL
021100020725     C+ open C1
021200020725     C/END-EXEC
021300020725     C
021400020725     C/EXEC SQL
021500070530     C+ Fetch C1 into :INPUT_DS
021600020725     C/END-EXEC
021700020725     C*
021800020725     C                   dow       sqlcod = *zeros
021900990913     C*
022000020725     C                   if        vindta > *blanks
022100000613     C                   add       1             rrnum
022200000801     C*
022300020725     C                   if        vinflg = *blanks
022400020725     C                             or vinflg = '0'
022500020725     C                             or vinflg = '2'
022600000801     C*
022700020725     C                   clear                   x_vinmsg
022800020725     C                   eval      x_vinflg = '1'
022900010305     C*
023000010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
023100110524     C                   EVAL      PiStr=%trim(%subst(vindta:322:8))
023200020305     C                   MOVEL(p)  PiStr         curspe
023300010305     C*
023400071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
023500080923     C                   if        *in50 = *off
023600071003     C                   exsr      impvab
023700071003     C                   exsr      wrivab
023800071003     C                   exsr      wrivat_b                                     => carico VAT
023900071003     C                   else
024000080923     C*
024100071009     C                   if        wDISK = 'D'
024200071009     C                   exsr      impvab
024300071009     C                   exsr      wrivab
024400071009     C                   exsr      wrivat_b                                     => carico VAT
024500071009     C                   exsr      wrivat_e                                     => carico VAT
024600071009     C                   else
024700071009     C*
024800010305     C                   if        depspe = *blanks                             => 1° giro
024900010305     C                   eval      depspe = curspe                              => memorizz. spediz
025000080117     C                   clear                   tivinds
025100020305     C                   exsr      impvab
025200071003     C                   exsr      wrivat_b                                     => carico VAT
025300071003     C   50              exsr      wrivat_e                                     => carico VAT
025400010305     C                   else
025500020725     C                   if        depspe <> curspe                             => rottura di spediz
025600010305     C                   eval      depspe = curspe                              => memorizz. spediz
025700070928     C                   exsr      wrivab
025800080117     C                   clear                   tivinds
025900020305     C                   exsr      impvab
026000071003     C                   exsr      wrivat_b                                     => carico VAT
026100071003     C   50              exsr      wrivat_e                                     => carico VAT
026200020305     C                   else                                                   => x stessa spediz
026300090204     C***                exsr      impvab
026400071003     C                   exsr      wrivat_b                                     => carico VAT
026500071003     C   50              exsr      wrivat_e                                     => carico VAT
026600010305     C                   endif
026700010305     C                   endif
026800010305     C                   endif
026900071003     C                   endif
027000071009     C                   endif
027100000905     C*
027200000905     C                   else
027300020725     C                   eval      x_vinflg = '1'
027400000905     C                   endif
027500000905     C*
027600020725     C     VINRRN        chain     tivin000
027700020725     C                   if        %found(tivin00r)
027800020725     C                   eval      y_vinflg = x_vinflg
027900020725     C                   eval      y_vinmsg = x_vinmsg
028000020725     C                   update    tivin000
028100020725     C                   endif
028200020725     C*
028300020725     C/EXEC SQL
028400070530     C+ Fetch C1 into :INPUT_DS
028500020725     C/END-EXEC
028600020725     C*
028700020725     C                   enddo
028800020725     C*
028900020725     C/EXEC SQL
029000020725     C+ close C1
029100020725     C/END-EXEC
029200000905     C*
029300020305     C* Scarico i VAB rimasti "in sospeso"
029400071009     C                   if        wDISK = 'C'
029500071009     C                   exsr      wrivab
029600071009     C                   endif
029700010202     C*
029800010202     C                   endif
029900990910
030000990910     C* Se non ci sono record con errori ...
030100000710     C                   if        §ctrno = 0
030200990910     C* ... restituisco esito OK.
030300990921     C                   eval      wrkesito = '0'
030400990910     C                   else
030500010201     C                   if        §ctrokvb > 0
030600990921     C                   eval      wrkesito = '1'
030700000710     C                   else
030800000710     C                   eval      wrkesito = '2'
030900990910     C                   endif
031000000710     C                   endif
031100990910     C*
031200990914     C                   if        %open(tivin00r)
031300990908     C                   close     tivin00r
031400990914     C                   endif
031500021113     C                   if        %open(fivabwwr)
031600021113     C                   close     fivabwwr
031700990914     C                   endif
031800021113     C                   if        %open(fivatwwr)
031900021113     C                   close     fivatwwr
032000010201     C                   endif
032100990910     C*
032200111031     C                   if        §ctrokvb > 0 or
032300111031     C                             §ctrokvt > 0
032400111031     C                             and wPOI <> *zeros
032500010202     C                   exsr      invio
032600990920     C                   endif
032700990920     C*
032800910830     C                   ENDSR
032900000613     C***
033000010305
033100010305     C*----------------------------------------------------*
033200020305     C*  SCARICAMENTO BUFFER RECORDS VAB
033300010305     C*----------------------------------------------------*
033400020305     C     WRIVAB        BEGSR
033500070730     C*
033600071003     C* Considerazioni finali
033700071003     C                   if        VABRMA = *blanks
033800071003     C                   movel(P)  VABRMN        VABRMA
033900071003     C                   endif
034000071003     C*
034100021113     C                   write     fivab000                                     => scarico il VAB
034200010305     C*
034300010305     C                   ENDSR
034400990920
034500000801     C*----------------------------------------------------*
034600000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
034700000801     C*----------------------------------------------------*
034800010201     C     INZVAR        BEGSR
034900000801     C*
035000010201     C                   Z-ADD     *zeros        Num5_0
035100020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
035200020725     C                   MOVEL     '0'           FlgCAS            1
035300000801     C*
035400000801     C                   ENDSR
035500000801     C*----------------------------------------------------*
035600000801     C*  IMPOSTAZIONE CAMPI COSTANTI
035700000801     C*----------------------------------------------------*
035800020904     C     DEFCAM        BEGSR
035900080923     C*
036000080923     C                   SETOFF                                       5051
036100000801     C*
036200021113     C                   CLEAR                   FIVAB000
036300021113     C                   CLEAR                   FIVAT000
036400070730     C* Imposto i valori di default...
036500110524     C                   EVAL      VABCCM = 0241160
036600110524     C                   EVAL      VATCCM = 0241160
036700110524     C                   EVAL      VABLNP = 024
036800110524     C                   EVAL      VATLNP = 024
036900101007     C                   EVAL      VABCTR = 000
037000070730     C                   EVAL      VABCBO = '1'
037100110520     C                   EVAL      VABTSP = 'C'
037200070222     C* ... e poi verifico se sono stati passati come parametri
037300070222     C                   IF        vlrppt > *blanks
037400070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
037500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
037600070222     C                   EXSR      CHKNUM
037700070222     C                   IF        PiInt=*on
037800070222     C                   Z-ADD     PiVal         VABCCM
037900070222     C                   Z-ADD     PiVal         VATCCM
038000070222     C                   ENDIF
038100070222     C                   ENDIF
038200070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
038300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
038400070222     C                   EXSR      CHKNUM
038500070222     C                   IF        PiInt=*on
038600070222     C                   Z-ADD     PiVal         VABLNP
038700070222     C                   Z-ADD     PiVal         VATLNP
038800070222     C                   ENDIF
038900070222     C                   ENDIF
039000070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
039100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
039200070222     C                   EXSR      CHKNUM
039300070222     C                   IF        PiInt=*on
039400070222     C                   Z-ADD     PiVal         VABCTR
039500070222     C                   ENDIF
039600070928     C                   ENDIF
039700071009     C                   MOVEL     *blanks       wDISK             1
039800071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
039900071009     C                   IF        wDISK <> *blanks
040000070928     C                   SETON                                        50
040100070222     C                   ENDIF
040200080923     C                   IF        %subst(vlrppt:15:1) = 'S'
040300080923     C                   SETON                                        51
040400080923     C                   ENDIF
040500070222     C                   ENDIF
040600071009     C*
040700071009     C   50              EVAL      VABCTM = '7Q'
040800000801     C*
040900000801     C                   ENDSR
041000000801     C*----------------------------------------------------*
041100021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
041200000801     C*----------------------------------------------------*
041300010201     C     IMPVAB        BEGSR
041400070530     C*
041500070530     C                   SETOFF                                       3132
041600070928     C*
041700070928     C                   EXSR      INZVAR
041800070928     C                   EXSR      DEFCAM
041900070928     C*
042000111031     C                   IF        vlrpoi <> 999
042100070928     C                   MOVE(P)   vlrpoi        VABFGS
042200070928     C                   MOVE(P)   vlrpoi        VATFGS
042300111031     C                   Z-ADD     vlrpoi        wPOI              3 0
042400111031     C                   ENDIF
042500070928     C*
042600070928     C                   MOVEL     datcor        VABAAS
042700070928     C                   MOVEL     datcor        VATAAS
042800070928     C                   MOVE      datcor        VABMGS
042900070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
043000070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
043100070928     C     '@':'A'       XLATE     VABRSD        VABRSD
043200070928     C* ==
043300070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
043400070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
043500070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
043600110524     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
043700110524     C**+                EVAL      VABRMA=%trim(%subst(vindta:331:10))
043800070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
043900080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
044000070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
044100070928     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
044200070928     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
044300100324     C***                IF        %trim(%subst(vindta:394:1)) = 'F'
044400100324     C***                EVAL      VABCBO='1'
044500100324     C***                ENDIF
044600100324     C***                IF        %trim(%subst(vindta:394:1)) = 'A'
044700100324     C***                EVAL      VABCBO='2'
044800100324     C***                ENDIF
044900100324     C***                IF        %trim(%subst(vindta:12:1)) = 'D'
045000100324     C***                EVAL      VABTSP='E'
045100100324     C***                ENDIF
045200100324     C***                IF        %trim(%subst(vindta:16:1)) = '1'
045300100324     C***                EVAL      VABFFD='S'
045400100324     C***                ENDIF
045500100324     C***                IF        %trim(%subst(vindta:16:1)) = '2'
045600100324     C***                EVAL      VABTC1='A'
045700100324     C***                ENDIF
045800111028     C* CCM
045900111028     C                   EVAL      PiStr=%trim(%subst(vindta:1:7))
046000111028     C                   EXSR      CHKNUM
046100111028     C                   IF        PiInt=*on
046200111028     C                   Z-ADD     PiVal         VABCCM
046300111028     C                   Z-ADD     PiVal         VATCCM
046400111028     C                   ELSE
046500111028     C                   SETON                                        32
046600111028     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046700111028     C                             + ' ' + 'VABCCM VATCCM'
046800111028     C                   ENDIF
046900111109     C* CTR
047000111109     C                   IF        VABCCM = 0240227
047100111109     C                   EVAL      VABCTR = 100
047200111109     C                   ENDIF
047300111028     C* LNP
047400111028     C                   EVAL      PiStr=%trim(%subst(vindta:14:3))
047500111028     C                   EXSR      CHKNUM
047600111028     C                   IF        PiInt=*on
047700111031     C                   Z-ADD     PiVal         VABLNP
047800111031     C                   Z-ADD     PiVal         VABFGS
047900111028     C                   Z-ADD     PiVal         VATLNP
048000111031     C                   Z-ADD     PiVal         VATFGS
048100111031     C*
048200111031     C                   IF        vlrpoi = 999
048300111031     C                   Z-ADD     VABFGS        wPOI
048400111031     C                   ENDIF
048500111031     C*
048600111028     C                   ELSE
048700111028     C                   SETON                                        32
048800111028     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048900111028     C                             + ' ' + 'VABLNP VATLNP'
049000111028     C                   ENDIF
049100071003     C* CAD
049200070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
049300070928     C                   EXSR      CHKNUM
049400070928     C                   IF        PiInt=*on
049500070928     C                   Z-ADD     PiVal         Num5_0
049600070928     C                   MOVEL     Num5_0        VABCAD
049700070928     C                   ELSE
049800070928     C                   SETON                                        32
049900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050000070928     C                             + ' ' + 'VABCAD'
050100070928     C                   ENDIF
050200080923     C* RMN
050300110524     C                   EVAL      PiStr=%trim(%subst(vindta:322:8))
050400110524     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
050500110223     C***                EVAL      PiStr=%trim(%subst(vindta:187:10))
050600101007     C***                EVAL      PiStr=%trim(%subst(vindta:333:8))
050700080923     C                   EXSR      CHKNUM
050800080923     C                   IF        PiInt=*on
050900080923     C                   Z-ADD     PiVal         VABRMN
051000080923     C                   ELSE
051100080923     C                   SETON                                        32
051200080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051300080923     C                             + ' ' + 'VABRMN'
051400080923     C                   ENDIF
051500080923     C* NSP
051600080923     C                   IF        *in51 = *on
051700080923     C* NSP => Stacco un numeratore da AZNUM
051800080923     C                   clear                   TRUL33DS
051900080923     C                   eval      I33OPE = *zeros
052000080923     C                   eval      I33CNU = 302
052100080923     C                   eval      I33NUM = 1
052200080923     C                   movel     TRUL33DS      KPJBU
052300080923     C                   call      'TRUL33R'
052400080923     C                   parm                    KPJBA
052500080923     C                   movel     KPJBU         TRUL33DS
052600080923     C                   if        O33ERR = *zeros
052700080923     C                   z-add     O33NRF        VABNSP
052800080923     C                   z-add     O33NRF        VATNSP
052900080923     C                   else
053000080923     C                   SETON                                        31
053100080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053200080923     C                             + ' ' + 'VABNSP VATNSP'
053300080923     C                   endif
053400080923     C                   ELSE
053500110525     C                   EVAL      PiStr=%trim(%subst(vindta:322:8))
053600110525     C***                EVAL      PiStr=%trim(%subst(vindta:189:7))
053700110223     C***                EVAL      PiStr=%trim(%subst(vindta:186+1:11-1))
053800080923     C                   EXSR      CHKNUM
053900080923     C                   IF        PiInt=*on
054000080923     C                   Z-ADD     PiVal         VABNSP
054100080923     C                   Z-ADD     PiVal         VATNSP
054200080923     C                   ELSE
054300080923     C                   SETON                                        31
054400080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054500080923     C                             + ' ' + 'VABNSP VATNSP'
054600080923     C                   ENDIF
054700080923     C                   ENDIF
054800071003     C* NCL
054900070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
055000070928     C                   EXSR      CHKNUM
055100070928     C                   IF        PiInt=*on
055200070928     C                   Z-ADD     PiVal         VABNCL
055300070928     C                   ELSE
055400070928     C                   SETON                                        32
055500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055600070928     C                             + ' ' + 'VABNCL'
055700070928     C                   ENDIF
055800071003     C* PKB
055900110520     C                   IF        %subst(vindta:170:8) <> *blanks
056000070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
056100070928     C                   EXSR      CHKNUM
056200070928     C                   IF        PiNum=*on
056300111124     C*
056400111124     C                   IF        VABCCM = 0240227
056500111124     C                   ADD       PiVal         VABPKB
056600111124     C                   ELSE
056700070928     C                   Z-ADD     PiVal         VABPKB
056800111124     C                   ENDIF
056900111124     C*
057000070928     C                   ELSE
057100070928     C                   SETON                                        32
057200070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057300070928     C                             + ' ' + 'VABPKB'
057400070928     C                   ENDIF
057500110520     C                   ELSE
057600110520     C                   IF        VABCTR = 100
057700110520     C                   EVAL      VABPKB = 18 * VABNCL
057800110520     C                   ELSE
057900110520     C                   EVAL      VABPKB = 10 * VABNCL
058000110520     C                   ENDIF
058100110520     C                   ENDIF
058200080923     C* CAS
058300080923     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
058400080923     C                             AND
058500080923     C                             %trim(%subst(vindta:280:12))<>*zeros
058600080923     C                             AND
058700080923     C                             %trim(%subst(vindta:280:12))<>'0,00'
058800080923     C                             AND
058900080923     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
059000080923     C                             AND
059100080923     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
059200070928     C                   MOVEL     '1'           FlgCAS
059300070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
059400070928     C                   EXSR      CHKNUM
059500070928     C                   IF        PiNum=*on
059600070928     C                   Z-ADD     PiVal         VABCAS
059700070928     C                   ELSE
059800070928     C                   SETON                                        32
059900070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
060000070928     C                             + ' ' + 'VABCAS'
060100070928     C                   ENDIF
060200070928     C                   ENDIF
060300071003     C* IAS
060400080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
060500080923     C                             AND
060600080923     C                             %trim(%subst(vindta:295:12))<>*zeros
060700080923     C                             AND
060800080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
060900080923     C                             AND
061000080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
061100080923     C                             AND
061200080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
061300080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
061400080923     C                   EXSR      CHKNUM
061500080923     C                   IF        PiNum=*on
061600080923     C                   Z-ADD     PiVal         VABIAS
061700080923     C                   ELSE
061800080923     C                   SETON                                        32
061900080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
062000080923     C                             + ' ' + 'VABIAS'
062100080923     C                   ENDIF
062200071003     C                   ENDIF
062300070730     C*
062400070730     C* Considerazioni finali su CBO/CAS
062500070730     C                   IF        FlgCAS = '1'
062600070730     C                   IF        VABCBO = '1'
062700070730     C                   EVAL      VABCBO = '4'
062800070730     C                   ENDIF
062900070730     C                   IF        VABCBO = '2'
063000070730     C                   EVAL      VABCBO = '6'
063100070730     C                   ENDIF
063200070730     C                   ENDIF
063300020305     C*
063400011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
063500011113     C                   EXSR      CHKIMPDIV
063600010202     C*
063700000801     C* Ebbene...
063800000801     C                   ADD       1             §CTRMO
063900070530     C                   IF        *in31 <> *zeros OR
064000070530     C                             *in32 <> *zeros
064100000801     C                   ADD       1             §CTRNO
064200000801     C                   EVAL      recko = vindta
064300000801     C                   EXCEPT    rigadett
064400020725     C                   EVAL      x_vinflg = '2'
064500000801     C                   ELSE
064600010201     C                   ADD       1             §CTROKVB
064700000801     C                   ENDIF
064800000801     C*
064900000801     C                   ENDSR
065000010201     C*----------------------------------------------------*
065100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
065200010201     C*----------------------------------------------------*
065300071003     C     WRIVAT_B      BEGSR
065400090204     C*
065500090204     C                   eval      vatAAS = vabAAS
065600090204     C                   eval      vatLNP = vabLNP
065700090204     C                   eval      vatNRS = vabNRS
065800090204     C                   eval      vatNSP = vabNSP
065900010201     C*
066000101007     C* Valorizzo il buffer di scrittura del FIVAT
066100070928     C* - TIPO RECORD "B"
066200070928     C                   if        %subst(vindta:92:15) <> *blanks
066300070928     C                   eval      VATTRC = 'B'
066400070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
066500021113     C                   write     FIVAT000
066600111031     C                   add       1             §ctrokvt
066700020725     C                   endif
066800010201     C*
066900010201     C                   ENDSR
067000071003     C*----------------------------------------------------*
067100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
067200071003     C*----------------------------------------------------*
067300071003     C     WRIVAT_E      BEGSR
067400090204     C*
067500090204     C                   eval      vatAAS = vabAAS
067600090204     C                   eval      vatLNP = vabLNP
067700090204     C                   eval      vatNRS = vabNRS
067800090204     C                   eval      vatNSP = vabNSP
067900071003     C*
068000101007     C* Valorizzo il buffer di scrittura del FIVAT
068100071003     C* - TIPO RECORD "E"
068200071003     C                   if        %subst(vindta:331:26) <> *blanks
068300071003     C                   eval      VATTRC = 'E'
068400071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
068500071003     C                   write     FIVAT000
068600111031     C                   add       1             §ctrokvt
068700071003     C                   endif
068800071003     C*
068900071003     C                   ENDSR
069000020904
069100020904
069200010202     C*----------------------------------------------------*
069300021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
069400010202     C*----------------------------------------------------*
069500020305     C     PREVAT        BEGSR
069600010202     C*
069700021113     C* Compongo il nome del membro da dare al FIVATWWR
069800010202     C                   eval      parmbr = vlrhdl
069900010202     C                   movel     'M'           parmbr
070000070530     C                   eval      parccm = %subst(vlrKSC:2:7)
070100010202     C                   eval      paropz = '1'
070200010202     C* Effettuo la chiamata al CLLE preposto
070300050128     C                   call(e)   'TITVVTC'
070400010202     C                   parm                    parccm
070500010202     C                   parm                    parmbr
070600010202     C                   parm                    paropz
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
072100110524     C                   EVAL      PiDecChr = '.'
072200081113     C                   ENDIF
072300081113     C*
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
073600000801
073700011113
073800011113     C*----------------------------------------------------*
073900011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
074000011113     C*----------------------------------------------------*
074100011113     C     CHKIMPDIV     BEGSR
074200011113     C*
074300011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
074400011113     C                   Z-ADD     *zeros        wrkDec            9 9
074500011113     C*
074600011113     C* Come prima cosa effettuo considerazioni sulla divisa
074700011113     C                   IF        vabIAS > *zeros
074800011113     C                   IF        vabVAS <> 'EUR'
074900011113     C                   EVAL      vabVAS =  'ITL'
075000011113     C                   ENDIF
075100011113     C                   ENDIF
075200011113     C*
075300011113     C                   IF        vabCAS > *zeros
075400011113     C                   IF        vabVCA <> 'EUR'
075500011113     C                   EVAL      vabVCA =  'ITL'
075600011113     C                   ENDIF
075700011113     C                   ENDIF
075800011113     C*
075900011113     C                   IF        vabVMD > *zeros
076000020305     C                   IF        vabVAD <> 'EUR'
076100011113     C                   EVAL      vabVAD =  'ITL'
076200011113     C                   ENDIF
076300011113     C                   ENDIF
076400011113     C*
076500011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
076600011113     C                   Z-ADD     vabIAS        wrkDec
076700011113     C                   IF        wrkDec > *zeros
076800011113     C                   IF        vabVAS = 'ITL'
076900011113     C                   EVAL      vabIAS = *zeros
077000011113     C                   ENDIF
077100011113     C                   ENDIF
077200011113     C*
077300011113     C* Stabilisco se il contrasegno ha decimali valorizzati
077400011113     C                   Z-ADD     vabCAS        wrkDec
077500011113     C                   IF        wrkDec > *zeros
077600011113     C                   IF        vabVCA = 'ITL'
077700011113     C                   EVAL      vabCAS = *zeros
077800011113     C                   ENDIF
077900011113     C                   ENDIF
078000011113     C*
078100011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
078200011113     C                   Z-ADD     vabVMD        wrkDec
078300011113     C                   IF        wrkDec > *zeros
078400011113     C                   IF        vabVAD = 'ITL'
078500011113     C                   EVAL      vabVMD = *zeros
078600011113     C                   ENDIF
078700011113     C                   ENDIF
078800011113     C*
078900011113     C                   ENDSR
079000011113     C***
079100011113
079200011113
079300000801
079400000801
079500990920      /TITLE Invio dei dati al punto operativo.
079600010202     C     invio         BEGSR
079700990920     C*
079800021113     C* 1° invio FIVAT
079900010201     C                   reset                   dscmz
080000111031     C                   move      wPOI          cmzdst
080100021113     C                   eval      cmzfld = 'FIVATWWR'
080200010201     C                   eval      cmzmbd = vlrhdl
080300010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
080400021009     C***                if        prmfir = *blanks
080500021113     C                   eval      cmzfla = 'FIVAT00F'
080600021113     C                   eval      cmzmba = 'FIVAT00F'
080700021009     C***                else
080800021009     C***                eval      cmzfla = prmfir
080900021009     C***                eval      cmzmba = prmfir
081000021009     C***                endif
081100010201     C                   eval      cmznrr = *zeros
081200020305     C                   move      §ctrokvt      cmznrr
081300021018     C                   eval      cmzlba = vlrfl1
081400010201     C                   call(e)   'TIS711C'
081500010201     C                   parm                    dscmz
081600010201     C                   parm      *blanks       esito
081700010205     C                   if        %error
081800010205     C                             or cmzerr = '1'
081900010205     C                             or esito  = '1'
082000010205     C                   eval      wrkesito = '3'
082100010205     C                   else
082200010201     C*
082300021113     C* 2° invio FIVAB
082400010201     C                   reset                   dscmz
082500111031     C                   move      wPOI          cmzdst
082600010201     C                   eval      cmzfld = vlrfou
082700010201     C                   eval      cmzmbd = vlrhdl
082800010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
082900021009     C***                if        prmfir = *blanks
083000021113     C                   eval      cmzfla = 'FIVAB00F'
083100021113     C                   eval      cmzmba = 'FIVAB00F'
083200021009     C***                else
083300021009     C***                eval      cmzfla = prmfir
083400021009     C***                eval      cmzmba = prmfir
083500021009     C***                endif
083600010201     C                   eval      cmznrr = *zeros
083700010201     C                   move      §ctrokvb      cmznrr
083800021018     C                   eval      cmzlba = vlrfl1
083900010201     C                   call(e)   'TIS711C'
084000010201     C                   parm                    dscmz
084100010201     C                   parm      *blanks       esito
084200010201     C                   if        %error
084300010201     C                             or cmzerr = '1'
084400010201     C                             or esito  = '1'
084500010201     C                   eval      wrkesito = '3'
084600010201     C                   endif
084700010205     C                   endif
084800990920     C*
084900000613     C                   ENDSR
085000000613     C***
085100070411
085200070411     C     *pssr         BEGSR
085300070411     C*
085400070411     C                   if        %open(tivin00r)
085500070411     C                   close     tivin00r
085600070411     C                   endif
085700070411     C                   if        %open(fivabwwr)
085800070411     C                   close     fivabwwr
085900070411     C                   endif
086000070411     C                   if        %open(fivatwwr)
086100070411     C                   close     fivatwwr
086200070411     C                   endif
086300070411     C*
086400070411     C* Effettuo la chiamata al CLLE preposto
086500070411     C                   call(e)   'TITVVTC'
086600070411     C                   parm                    parccm
086700070411     C                   parm                    parmbr
086800070411     C                   parm      '2'           paropz
086900070411     C*
087000070411     C                   eval      wrkesito = '2'
087100070411     C*
087200070411     C                   seton                                        LR
087300070411     C*
087400070411     C                   ENDSR     '*CANCL'
087500070411     C***
087600070411
087700990910
087800000613     C     *inzsr        BEGSR
087900990910     C*
088000990910     C     *entry        plist
088100990920     C                   parm                    tivlrds
088200990921     C                   parm      wrkesito      esito
088300000724     C                   parm                    prmlit
088400000710     C                   parm                    prmfir
088500000613     C*
088600000830     C* CALCOLA LA DATA CORRENTE
088700100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
088800100324     C                   eval      datcor = %dec(%date() : *ISO)
088900000830     C*
089000000613     C                   ENDSR
089100000613     C***
089200990908
089300110524     Otitv2f8p  E            riepilogo         2
089400990915     O                                              'Upload via Internet'
089500990915     O                                           +1 'Traduzione TIVIN00R -'
089600021113     O                                           +1 'FIVABWWR/FIVATWWR'
089700010201     O                                           +1 'Report testate bolle'
089800990915     O          E            riepilogo   2
089900990915     O                       wrkdata
090000990915     O                       wrkora              +1
090100990915     O                       procname            +1
090200990915     O          E            riepilogo   2
090300990915     O                                              'Cliente..................:'
090400990915     O                       VABCCM        z     +1
090500990915     O          E            riepilogo   2
090600990920     O                                              'Riferimento Strategi.....:'
090700990920     O                       vlrhdl              +1
090800990915     O          E            riepilogo   2
090900990915     O                                              'Giusti...................:'
091000010201     O                       §CTROKVB      2   +  1
091100990915     O          E            riepilogo   2
091200990915     O                                              'Sbagliati e corretti.....:'
091300971022     O                       §CTRMO        2   +  1
091400990915     O          E            riepilogo   2
091500990915     O                                              'Sbagliati e scartati.....:'
091600971022     O                       §CTRNO        2   +  1
091700000613
091800110524     Otitv2f8ps E            testdett          2
091900000613     O                                              'Upload via Internet'
092000000613     O                                           +1 'Traduzione TIVIN00R -'
092100021113     O                                           +1 'FIVABWWR/FIVATWWR'
092200010201     O                                           +1 'Report testate bolle'
092300000616     O          E            testdett    3
092400000613     O                                           +2 'N° rec'
092500000613     O                                           +3 'Anteprima contenuto'
092600000616     O          E            rigadett    2
092700000613     O                       rrnum               +2
092800000621     O                       recko               +3
092900000616     O          E            findett     2
093000000613     O                       wrkdata
093100000613     O                       wrkora              +1
093200000613     O                       procname            +1
093300000616     O          E            findett     2
093400000613     O                                              'Cliente..................:'
093500000613     O                       VABCCM        z     +1
093600000616     O          E            findett     2
093700000613     O                                              'Riferimento Strategi.....:'
093800000613     O                       vlrhdl              +1
093900000616     O          E            findett     2
094000000613     O                                              'Giusti...................:'
094100010201     O                       §CTROKVB      2   +  1
094200000616     O          E            findett     2
094300000613     O                                              'Sbagliati e corretti.....:'
094400000613     O                       §CTRMO        2   +  1
094500000616     O          E            findett     2
094600000613     O                                              'Sbagliati e scartati.....:'
094700000613     O                       §CTRNO        2   +  1
094800000512** CMD - COMANDI CL
094900110524OVRPRTF FILE(TITV2F8P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
095000110524OVRPRTF FILE(TITV2F8PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
095100110524DLTOVR FILE(TITV2F8P TITV2F8PS) LVL(*)
095200000512
095300000512
