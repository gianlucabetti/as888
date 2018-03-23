000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800110613     Ftitv2g0p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100110613     Ftitv2g0ps O    f  198        PRINTER usropn
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
011600110613     C                   if        not %open(titv2g0ps)
011700110613     C                   open      titv2g0ps
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
013600110613     C                   if        %open(titv2g0ps)
013700000616     C                   except    findett
013800110613     C                   close     titv2g0ps
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
015500110613     C                   if        not %open(titv2g0p)
015600110613     C                   open      titv2g0p
015700990915     C                   endif
015800990915     C*
015900990915     C                   except    riepilogo
016000990915     C*
016100110613     C                   if        %open(titv2g0p)
016200110613     C                   close     titv2g0p
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
020000110524     C+ vindta, vinflg, substr(vindta, 322, 8) as sped, rrn(tivin00r)
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
022700110524     C                   EVAL      PiStr=%trim(%subst(vindta:322:8))
022800020305     C                   MOVEL(p)  PiStr         curspe
022900010305     C*
023000071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
023100080923     C                   if        *in50 = *off
023200071003     C                   exsr      impvab
023300071003     C                   exsr      wrivab
023400071003     C                   exsr      wrivat_b                                     => carico VAT
023500071003     C                   else
023600080923     C*
023700071009     C                   if        wDISK = 'D'
023800071009     C                   exsr      impvab
023900071009     C                   exsr      wrivab
024000071009     C                   exsr      wrivat_b                                     => carico VAT
024100071009     C                   exsr      wrivat_e                                     => carico VAT
024200071009     C                   else
024300071009     C*
024400010305     C                   if        depspe = *blanks                             => 1° giro
024500010305     C                   eval      depspe = curspe                              => memorizz. spediz
024600080117     C                   clear                   tivinds
024700020305     C                   exsr      impvab
024800071003     C                   exsr      wrivat_b                                     => carico VAT
024900071003     C   50              exsr      wrivat_e                                     => carico VAT
025000010305     C                   else
025100020725     C                   if        depspe <> curspe                             => rottura di spediz
025200010305     C                   eval      depspe = curspe                              => memorizz. spediz
025300070928     C                   exsr      wrivab
025400080117     C                   clear                   tivinds
025500020305     C                   exsr      impvab
025600071003     C                   exsr      wrivat_b                                     => carico VAT
025700071003     C   50              exsr      wrivat_e                                     => carico VAT
025800020305     C                   else                                                   => x stessa spediz
025900090204     C***                exsr      impvab
026000071003     C                   exsr      wrivat_b                                     => carico VAT
026100071003     C   50              exsr      wrivat_e                                     => carico VAT
026200010305     C                   endif
026300010305     C                   endif
026400010305     C                   endif
026500071003     C                   endif
026600071009     C                   endif
026700000905     C*
026800000905     C                   else
026900020725     C                   eval      x_vinflg = '1'
027000000905     C                   endif
027100000905     C*
027200020725     C     VINRRN        chain     tivin000
027300020725     C                   if        %found(tivin00r)
027400020725     C                   eval      y_vinflg = x_vinflg
027500020725     C                   eval      y_vinmsg = x_vinmsg
027600020725     C                   update    tivin000
027700020725     C                   endif
027800020725     C*
027900020725     C/EXEC SQL
028000070530     C+ Fetch C1 into :INPUT_DS
028100020725     C/END-EXEC
028200020725     C*
028300020725     C                   enddo
028400020725     C*
028500020725     C/EXEC SQL
028600020725     C+ close C1
028700020725     C/END-EXEC
028800000905     C*
028900020305     C* Scarico i VAB rimasti "in sospeso"
029000071009     C                   if        wDISK = 'C'
029100071009     C                   exsr      wrivab
029200071009     C                   endif
029300010202     C*
029400010202     C                   endif
029500990910
029600990910     C* Se non ci sono record con errori ...
029700000710     C                   if        §ctrno = 0
029800990910     C* ... restituisco esito OK.
029900990921     C                   eval      wrkesito = '0'
030000990910     C                   else
030100010201     C                   if        §ctrokvb > 0
030200990921     C                   eval      wrkesito = '1'
030300000710     C                   else
030400000710     C                   eval      wrkesito = '2'
030500990910     C                   endif
030600000710     C                   endif
030700990910     C*
030800990914     C                   if        %open(tivin00r)
030900990908     C                   close     tivin00r
031000990914     C                   endif
031100021113     C                   if        %open(fivabwwr)
031200021113     C                   close     fivabwwr
031300990914     C                   endif
031400021113     C                   if        %open(fivatwwr)
031500021113     C                   close     fivatwwr
031600010201     C                   endif
031700990910     C*
031800010201     C                   if        §ctrokvb > 0
031900000724     C                             and vlrpoi <> *zeros
032000010202     C                   exsr      invio
032100990920     C                   endif
032200990920     C*
032300910830     C                   ENDSR
032400000613     C***
032500010305
032600010305     C*----------------------------------------------------*
032700020305     C*  SCARICAMENTO BUFFER RECORDS VAB
032800010305     C*----------------------------------------------------*
032900020305     C     WRIVAB        BEGSR
033000070730     C*
033100071003     C* Considerazioni finali
033200071003     C                   if        VABRMA = *blanks
033300071003     C                   movel(P)  VABRMN        VABRMA
033400071003     C                   endif
033500071003     C*
033600021113     C                   write     fivab000                                     => scarico il VAB
033700010305     C*
033800010305     C                   ENDSR
033900990920
034000000801     C*----------------------------------------------------*
034100000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
034200000801     C*----------------------------------------------------*
034300010201     C     INZVAR        BEGSR
034400000801     C*
034500010201     C                   Z-ADD     *zeros        Num5_0
034600020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
034700020725     C                   MOVEL     '0'           FlgCAS            1
034800000801     C*
034900000801     C                   ENDSR
035000000801     C*----------------------------------------------------*
035100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
035200000801     C*----------------------------------------------------*
035300020904     C     DEFCAM        BEGSR
035400080923     C*
035500080923     C                   SETOFF                                       5051
035600000801     C*
035700021113     C                   CLEAR                   FIVAB000
035800021113     C                   CLEAR                   FIVAT000
035900070730     C* Imposto i valori di default...
036000110613     C                   EVAL      VABCCM = 0182706
036100110613     C                   EVAL      VATCCM = 0182706
036200110613     C                   EVAL      VABLNP = 018
036300110613     C                   EVAL      VATLNP = 018
036400110613     C                   EVAL      VABCTR = 305
036500070730     C                   EVAL      VABCBO = '1'
036600110520     C                   EVAL      VABTSP = 'C'
036700070222     C* ... e poi verifico se sono stati passati come parametri
036800070222     C                   IF        vlrppt > *blanks
036900070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
037000070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
037100070222     C                   EXSR      CHKNUM
037200070222     C                   IF        PiInt=*on
037300070222     C                   Z-ADD     PiVal         VABCCM
037400070222     C                   Z-ADD     PiVal         VATCCM
037500070222     C                   ENDIF
037600070222     C                   ENDIF
037700070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
037800070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
037900070222     C                   EXSR      CHKNUM
038000070222     C                   IF        PiInt=*on
038100070222     C                   Z-ADD     PiVal         VABLNP
038200070222     C                   Z-ADD     PiVal         VATLNP
038300070222     C                   ENDIF
038400070222     C                   ENDIF
038500070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
038600070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
038700070222     C                   EXSR      CHKNUM
038800070222     C                   IF        PiInt=*on
038900070222     C                   Z-ADD     PiVal         VABCTR
039000070222     C                   ENDIF
039100070928     C                   ENDIF
039200071009     C                   MOVEL     *blanks       wDISK             1
039300071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
039400071009     C                   IF        wDISK <> *blanks
039500070928     C                   SETON                                        50
039600070222     C                   ENDIF
039700080923     C                   IF        %subst(vlrppt:15:1) = 'S'
039800080923     C                   SETON                                        51
039900080923     C                   ENDIF
040000070222     C                   ENDIF
040100071009     C*
040200071009     C   50              EVAL      VABCTM = '7Q'
040300000801     C*
040400000801     C                   ENDSR
040500000801     C*----------------------------------------------------*
040600021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
040700000801     C*----------------------------------------------------*
040800010201     C     IMPVAB        BEGSR
040900070530     C*
041000070530     C                   SETOFF                                       3132
041100070928     C*
041200070928     C                   EXSR      INZVAR
041300070928     C                   EXSR      DEFCAM
041400070928     C*
041500070928     C                   MOVE(P)   vlrpoi        VABFGS
041600070928     C                   MOVE(P)   vlrpoi        VATFGS
041700070928     C*
041800070928     C                   MOVEL     datcor        VABAAS
041900070928     C                   MOVEL     datcor        VATAAS
042000070928     C                   MOVE      datcor        VABMGS
042100070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
042200070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
042300070928     C     '@':'A'       XLATE     VABRSD        VABRSD
042400070928     C* ==
042500070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
042600070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
042700070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
042800110524     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
042900110524     C**+                EVAL      VABRMA=%trim(%subst(vindta:331:10))
043000070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
043100080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
043200070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
043300070928     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
043400070928     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
043500100324     C***                IF        %trim(%subst(vindta:394:1)) = 'F'
043600100324     C***                EVAL      VABCBO='1'
043700100324     C***                ENDIF
043800100324     C***                IF        %trim(%subst(vindta:394:1)) = 'A'
043900100324     C***                EVAL      VABCBO='2'
044000100324     C***                ENDIF
044100100324     C***                IF        %trim(%subst(vindta:12:1)) = 'D'
044200100324     C***                EVAL      VABTSP='E'
044300100324     C***                ENDIF
044400100324     C***                IF        %trim(%subst(vindta:16:1)) = '1'
044500100324     C***                EVAL      VABFFD='S'
044600100324     C***                ENDIF
044700100324     C***                IF        %trim(%subst(vindta:16:1)) = '2'
044800100324     C***                EVAL      VABTC1='A'
044900100324     C***                ENDIF
045000071003     C* CAD
045100070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
045200070928     C                   EXSR      CHKNUM
045300070928     C                   IF        PiInt=*on
045400070928     C                   Z-ADD     PiVal         Num5_0
045500070928     C                   MOVEL     Num5_0        VABCAD
045600070928     C                   ELSE
045700070928     C                   SETON                                        32
045800070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045900070928     C                             + ' ' + 'VABCAD'
046000070928     C                   ENDIF
046100080923     C* RMN
046200110613     C***                EVAL      PiStr=%trim(%subst(vindta:322:8))
046300110613     C                   EVAL      PiStr=%trim(%subst(vindta:189:7))
046400110223     C***                EVAL      PiStr=%trim(%subst(vindta:187:10))
046500101007     C***                EVAL      PiStr=%trim(%subst(vindta:333:8))
046600080923     C                   EXSR      CHKNUM
046700080923     C                   IF        PiInt=*on
046800080923     C                   Z-ADD     PiVal         VABRMN
046900080923     C                   ELSE
047000080923     C                   SETON                                        32
047100080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047200080923     C                             + ' ' + 'VABRMN'
047300080923     C                   ENDIF
047400080923     C* NSP
047500080923     C                   IF        *in51 = *on
047600080923     C* NSP => Stacco un numeratore da AZNUM
047700080923     C                   clear                   TRUL33DS
047800080923     C                   eval      I33OPE = *zeros
047900080923     C                   eval      I33CNU = 302
048000080923     C                   eval      I33NUM = 1
048100080923     C                   movel     TRUL33DS      KPJBU
048200080923     C                   call      'TRUL33R'
048300080923     C                   parm                    KPJBA
048400080923     C                   movel     KPJBU         TRUL33DS
048500080923     C                   if        O33ERR = *zeros
048600080923     C                   z-add     O33NRF        VABNSP
048700080923     C                   z-add     O33NRF        VATNSP
048800080923     C                   else
048900080923     C                   SETON                                        31
049000080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049100080923     C                             + ' ' + 'VABNSP VATNSP'
049200080923     C                   endif
049300080923     C                   ELSE
049400110613     C***                EVAL      PiStr=%trim(%subst(vindta:322:8))
049500110613     C                   EVAL      PiStr=%trim(%subst(vindta:189:7))
049600110223     C***                EVAL      PiStr=%trim(%subst(vindta:186+1:11-1))
049700080923     C                   EXSR      CHKNUM
049800080923     C                   IF        PiInt=*on
049900080923     C                   Z-ADD     PiVal         VABNSP
050000080923     C                   Z-ADD     PiVal         VATNSP
050100080923     C                   ELSE
050200080923     C                   SETON                                        31
050300080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050400080923     C                             + ' ' + 'VABNSP VATNSP'
050500080923     C                   ENDIF
050600080923     C                   ENDIF
050700071003     C* NCL
050800070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
050900070928     C                   EXSR      CHKNUM
051000070928     C                   IF        PiInt=*on
051100070928     C                   Z-ADD     PiVal         VABNCL
051200070928     C                   ELSE
051300070928     C                   SETON                                        32
051400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051500070928     C                             + ' ' + 'VABNCL'
051600070928     C                   ENDIF
051700071003     C* PKB
051800110520     C                   IF        %subst(vindta:170:8) <> *blanks
051900070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
052000070928     C                   EXSR      CHKNUM
052100070928     C                   IF        PiNum=*on
052200070928     C                   Z-ADD     PiVal         VABPKB
052300070928     C                   ELSE
052400070928     C                   SETON                                        32
052500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052600070928     C                             + ' ' + 'VABPKB'
052700070928     C                   ENDIF
052800110520     C                   ELSE
052900110520     C                   IF        VABCTR = 100
053000110520     C                   EVAL      VABPKB = 18 * VABNCL
053100110520     C                   ELSE
053200110520     C                   EVAL      VABPKB = 10 * VABNCL
053300110520     C                   ENDIF
053400110520     C                   ENDIF
053500080923     C* CAS
053600080923     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
053700080923     C                             AND
053800080923     C                             %trim(%subst(vindta:280:12))<>*zeros
053900080923     C                             AND
054000080923     C                             %trim(%subst(vindta:280:12))<>'0,00'
054100080923     C                             AND
054200080923     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
054300080923     C                             AND
054400080923     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
054500070928     C                   MOVEL     '1'           FlgCAS
054600070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
054700070928     C                   EXSR      CHKNUM
054800070928     C                   IF        PiNum=*on
054900070928     C                   Z-ADD     PiVal         VABCAS
055000070928     C                   ELSE
055100070928     C                   SETON                                        32
055200070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
055300070928     C                             + ' ' + 'VABCAS'
055400070928     C                   ENDIF
055500070928     C                   ENDIF
055600071003     C* IAS
055700080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
055800080923     C                             AND
055900080923     C                             %trim(%subst(vindta:295:12))<>*zeros
056000080923     C                             AND
056100080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
056200080923     C                             AND
056300080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
056400080923     C                             AND
056500080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
056600080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
056700080923     C                   EXSR      CHKNUM
056800080923     C                   IF        PiNum=*on
056900080923     C                   Z-ADD     PiVal         VABIAS
057000080923     C                   ELSE
057100080923     C                   SETON                                        32
057200080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
057300080923     C                             + ' ' + 'VABIAS'
057400080923     C                   ENDIF
057500071003     C                   ENDIF
057600070730     C*
057700070730     C* Considerazioni finali su CBO/CAS
057800070730     C                   IF        FlgCAS = '1'
057900070730     C                   IF        VABCBO = '1'
058000070730     C                   EVAL      VABCBO = '4'
058100070730     C                   ENDIF
058200070730     C                   IF        VABCBO = '2'
058300070730     C                   EVAL      VABCBO = '6'
058400070730     C                   ENDIF
058500070730     C                   ENDIF
058600020305     C*
058700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
058800011113     C                   EXSR      CHKIMPDIV
058900010202     C*
059000000801     C* Ebbene...
059100000801     C                   ADD       1             §CTRMO
059200070530     C                   IF        *in31 <> *zeros OR
059300070530     C                             *in32 <> *zeros
059400000801     C                   ADD       1             §CTRNO
059500000801     C                   EVAL      recko = vindta
059600000801     C                   EXCEPT    rigadett
059700020725     C                   EVAL      x_vinflg = '2'
059800000801     C                   ELSE
059900010201     C                   ADD       1             §CTROKVB
060000000801     C                   ENDIF
060100000801     C*
060200000801     C                   ENDSR
060300010201     C*----------------------------------------------------*
060400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
060500010201     C*----------------------------------------------------*
060600071003     C     WRIVAT_B      BEGSR
060700090204     C*
060800090204     C                   eval      vatAAS = vabAAS
060900090204     C                   eval      vatLNP = vabLNP
061000090204     C                   eval      vatNRS = vabNRS
061100090204     C                   eval      vatNSP = vabNSP
061200010201     C*
061300101007     C* Valorizzo il buffer di scrittura del FIVAT
061400070928     C* - TIPO RECORD "B"
061500070928     C                   if        %subst(vindta:92:15) <> *blanks
061600070928     C                   eval      VATTRC = 'B'
061700070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
061800021113     C                   write     FIVAT000
061900020725     C                   endif
062000010201     C*
062100010201     C                   ENDSR
062200071003     C*----------------------------------------------------*
062300071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
062400071003     C*----------------------------------------------------*
062500071003     C     WRIVAT_E      BEGSR
062600090204     C*
062700090204     C                   eval      vatAAS = vabAAS
062800090204     C                   eval      vatLNP = vabLNP
062900090204     C                   eval      vatNRS = vabNRS
063000090204     C                   eval      vatNSP = vabNSP
063100071003     C*
063200101007     C* Valorizzo il buffer di scrittura del FIVAT
063300071003     C* - TIPO RECORD "E"
063400071003     C                   if        %subst(vindta:331:26) <> *blanks
063500071003     C                   eval      VATTRC = 'E'
063600071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
063700071003     C                   write     FIVAT000
063800071003     C                   endif
063900071003     C*
064000071003     C                   ENDSR
064100020904
064200020904
064300010202     C*----------------------------------------------------*
064400021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
064500010202     C*----------------------------------------------------*
064600020305     C     PREVAT        BEGSR
064700010202     C*
064800021113     C* Compongo il nome del membro da dare al FIVATWWR
064900010202     C                   eval      parmbr = vlrhdl
065000010202     C                   movel     'M'           parmbr
065100070530     C                   eval      parccm = %subst(vlrKSC:2:7)
065200010202     C                   eval      paropz = '1'
065300010202     C* Effettuo la chiamata al CLLE preposto
065400050128     C                   call(e)   'TITVVTC'
065500010202     C                   parm                    parccm
065600010202     C                   parm                    parmbr
065700010202     C                   parm                    paropz
065800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
065900010202     C                   if        %error
066000010202     C                   movel     '1'           chkcall
066100010202     C                   else
066200010202     C                   movel     '0'           chkcall
066300010202     C                   endif
066400010202     C*
066500010202     C                   ENDSR
066600000801     C*----------------------------------------------------*
066700000801     C*  CONTROLLO NUMERICITA' CAMPI
066800000801     C*----------------------------------------------------*
066900000801     C     CHKNUM        BEGSR
067000081113     C*
067100081113     C                   IF        PiDecChr = *blanks
067200110524     C                   EVAL      PiDecChr = '.'
067300081113     C                   ENDIF
067400081113     C*
067500081113     C                   callp(e)  UBISNUM_Check(PiStr
067600081113     C                                          :PiDecChr
067700081113     C                                          :PiVal
067800081113     C                                          :PiNum
067900081113     C                                          :PiInt)
068000081113     C*
068100000801     C                   IF        %error
068200000801     C                   EVAL      PiInt=*off
068300000801     C                   ENDIF
068400000801     C*
068500000801     C                   ENDSR
068600000801     C***
068700000801
068800011113
068900011113     C*----------------------------------------------------*
069000011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
069100011113     C*----------------------------------------------------*
069200011113     C     CHKIMPDIV     BEGSR
069300011113     C*
069400011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
069500011113     C                   Z-ADD     *zeros        wrkDec            9 9
069600011113     C*
069700011113     C* Come prima cosa effettuo considerazioni sulla divisa
069800011113     C                   IF        vabIAS > *zeros
069900011113     C                   IF        vabVAS <> 'EUR'
070000011113     C                   EVAL      vabVAS =  'ITL'
070100011113     C                   ENDIF
070200011113     C                   ENDIF
070300011113     C*
070400011113     C                   IF        vabCAS > *zeros
070500011113     C                   IF        vabVCA <> 'EUR'
070600011113     C                   EVAL      vabVCA =  'ITL'
070700011113     C                   ENDIF
070800011113     C                   ENDIF
070900011113     C*
071000011113     C                   IF        vabVMD > *zeros
071100020305     C                   IF        vabVAD <> 'EUR'
071200011113     C                   EVAL      vabVAD =  'ITL'
071300011113     C                   ENDIF
071400011113     C                   ENDIF
071500011113     C*
071600011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
071700011113     C                   Z-ADD     vabIAS        wrkDec
071800011113     C                   IF        wrkDec > *zeros
071900011113     C                   IF        vabVAS = 'ITL'
072000011113     C                   EVAL      vabIAS = *zeros
072100011113     C                   ENDIF
072200011113     C                   ENDIF
072300011113     C*
072400011113     C* Stabilisco se il contrasegno ha decimali valorizzati
072500011113     C                   Z-ADD     vabCAS        wrkDec
072600011113     C                   IF        wrkDec > *zeros
072700011113     C                   IF        vabVCA = 'ITL'
072800011113     C                   EVAL      vabCAS = *zeros
072900011113     C                   ENDIF
073000011113     C                   ENDIF
073100011113     C*
073200011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
073300011113     C                   Z-ADD     vabVMD        wrkDec
073400011113     C                   IF        wrkDec > *zeros
073500011113     C                   IF        vabVAD = 'ITL'
073600011113     C                   EVAL      vabVMD = *zeros
073700011113     C                   ENDIF
073800011113     C                   ENDIF
073900011113     C*
074000011113     C                   ENDSR
074100011113     C***
074200011113
074300011113
074400000801
074500000801
074600990920      /TITLE Invio dei dati al punto operativo.
074700010202     C     invio         BEGSR
074800990920     C*
074900021113     C* 1° invio FIVAT
075000010201     C                   reset                   dscmz
075100010201     C                   move      vlrpoi        cmzdst
075200021113     C                   eval      cmzfld = 'FIVATWWR'
075300010201     C                   eval      cmzmbd = vlrhdl
075400010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075500021009     C***                if        prmfir = *blanks
075600021113     C                   eval      cmzfla = 'FIVAT00F'
075700021113     C                   eval      cmzmba = 'FIVAT00F'
075800021009     C***                else
075900021009     C***                eval      cmzfla = prmfir
076000021009     C***                eval      cmzmba = prmfir
076100021009     C***                endif
076200010201     C                   eval      cmznrr = *zeros
076300020305     C                   move      §ctrokvt      cmznrr
076400021018     C                   eval      cmzlba = vlrfl1
076500010201     C                   call(e)   'TIS711C'
076600010201     C                   parm                    dscmz
076700010201     C                   parm      *blanks       esito
076800010205     C                   if        %error
076900010205     C                             or cmzerr = '1'
077000010205     C                             or esito  = '1'
077100010205     C                   eval      wrkesito = '3'
077200010205     C                   else
077300010201     C*
077400021113     C* 2° invio FIVAB
077500010201     C                   reset                   dscmz
077600010201     C                   move      vlrpoi        cmzdst
077700010201     C                   eval      cmzfld = vlrfou
077800010201     C                   eval      cmzmbd = vlrhdl
077900010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078000021009     C***                if        prmfir = *blanks
078100021113     C                   eval      cmzfla = 'FIVAB00F'
078200021113     C                   eval      cmzmba = 'FIVAB00F'
078300021009     C***                else
078400021009     C***                eval      cmzfla = prmfir
078500021009     C***                eval      cmzmba = prmfir
078600021009     C***                endif
078700010201     C                   eval      cmznrr = *zeros
078800010201     C                   move      §ctrokvb      cmznrr
078900021018     C                   eval      cmzlba = vlrfl1
079000010201     C                   call(e)   'TIS711C'
079100010201     C                   parm                    dscmz
079200010201     C                   parm      *blanks       esito
079300010201     C                   if        %error
079400010201     C                             or cmzerr = '1'
079500010201     C                             or esito  = '1'
079600010201     C                   eval      wrkesito = '3'
079700010201     C                   endif
079800010205     C                   endif
079900990920     C*
080000000613     C                   ENDSR
080100000613     C***
080200070411
080300070411     C     *pssr         BEGSR
080400070411     C*
080500070411     C                   if        %open(tivin00r)
080600070411     C                   close     tivin00r
080700070411     C                   endif
080800070411     C                   if        %open(fivabwwr)
080900070411     C                   close     fivabwwr
081000070411     C                   endif
081100070411     C                   if        %open(fivatwwr)
081200070411     C                   close     fivatwwr
081300070411     C                   endif
081400070411     C*
081500070411     C* Effettuo la chiamata al CLLE preposto
081600070411     C                   call(e)   'TITVVTC'
081700070411     C                   parm                    parccm
081800070411     C                   parm                    parmbr
081900070411     C                   parm      '2'           paropz
082000070411     C*
082100070411     C                   eval      wrkesito = '2'
082200070411     C*
082300070411     C                   seton                                        LR
082400070411     C*
082500070411     C                   ENDSR     '*CANCL'
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
083800100324     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
083900100324     C                   eval      datcor = %dec(%date() : *ISO)
084000000830     C*
084100000613     C                   ENDSR
084200000613     C***
084300990908
084400110613     Otitv2g0p  E            riepilogo         2
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
086200990915     O          E            riepilogo   2
086300990915     O                                              'Sbagliati e corretti.....:'
086400971022     O                       §CTRMO        2   +  1
086500990915     O          E            riepilogo   2
086600990915     O                                              'Sbagliati e scartati.....:'
086700971022     O                       §CTRNO        2   +  1
086800000613
086900110613     Otitv2g0ps E            testdett          2
087000000613     O                                              'Upload via Internet'
087100000613     O                                           +1 'Traduzione TIVIN00R -'
087200021113     O                                           +1 'FIVABWWR/FIVATWWR'
087300010201     O                                           +1 'Report testate bolle'
087400000616     O          E            testdett    3
087500000613     O                                           +2 'N° rec'
087600000613     O                                           +3 'Anteprima contenuto'
087700000616     O          E            rigadett    2
087800000613     O                       rrnum               +2
087900000621     O                       recko               +3
088000000616     O          E            findett     2
088100000613     O                       wrkdata
088200000613     O                       wrkora              +1
088300000613     O                       procname            +1
088400000616     O          E            findett     2
088500000613     O                                              'Cliente..................:'
088600000613     O                       VABCCM        z     +1
088700000616     O          E            findett     2
088800000613     O                                              'Riferimento Strategi.....:'
088900000613     O                       vlrhdl              +1
089000000616     O          E            findett     2
089100000613     O                                              'Giusti...................:'
089200010201     O                       §CTROKVB      2   +  1
089300000616     O          E            findett     2
089400000613     O                                              'Sbagliati e corretti.....:'
089500000613     O                       §CTRMO        2   +  1
089600000616     O          E            findett     2
089700000613     O                                              'Sbagliati e scartati.....:'
089800000613     O                       §CTRNO        2   +  1
089900000512** CMD - COMANDI CL
090000110613OVRPRTF FILE(TITV2G0P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
090100110613OVRPRTF FILE(TITV2G0PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
090200110613DLTOVR FILE(TITV2G0P TITV2G0PS) LVL(*)
090300000512
090400000512
