000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800090218     Ftitv1m9p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100090218     Ftitv1m9ps O    f  198        PRINTER usropn
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
003600070928     D depspe          s             16    INZ(*blanks)
003700070928     D curspe          s             16    INZ(*blanks)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200080117     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004300070530     D Num5_0          s              5  0
004400000830
004500000830     D*------------------
004600000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004700000830     D*------------------
004800000830     D WLBDA8          DS                  INZ
004900000830     D  G08DAT                 1      8  0
005000000830     D  G08INV                 9     16  0
005100000830     D  G08ERR                17     17
005200000830     D  G08TGI                18     22  0
005300000830     D*
005400020725
005500020725     D*------------------
005600020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
005700020725     D*------------------
005800070530     D INPUT_DS        DS                  INZ
005900020725     D  VINDTA                 1   2048
006000020725     D  VINFLG              2049   2049
006100020725     D  VINSPE              2050   2059
006200020725     D  VINRRN              2060   2067  0
006300020725     D*
006400081113
006500081113
006600081113     D*------------------
006700081113     D* LINKING A DEFINIZIONI ESTERNE
006800081113     D*------------------
006900081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007000090223     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007100081113
007200990908
007300010201
007400010201
007500080117     C*
007600080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
007700080117     C
007800080117     C/EXEC SQL
007900080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
008000080117     C/END-EXEC
008100080117     C
008200990915     C                   time                    wrkdata
008300990915     C                   time                    wrkora
008400000913     C                   reset                   rrnum
008500990921     C                   reset                   esito
008600990921     C                   reset                   wrkesito
008700000724     C*
008800000724     C* SE OCCORRE SPEDIRE IN FILIALE
008900000724     C                   if        vlrpoi <> *zeros
009000000724     C*
009100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009300000724     C     vlrpoi        chain     azorg01l
009400000724     C                   if        %found
009500000616     C                   movel(p)  CMD(1)        depcmd
009600020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009700000616     C*
009800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009900011113     C                   Z-ADD     150           LENGH            15 5
010000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010100000616     C                   PARM                    depcmd
010200000616     C                   PARM                    LENGH
010300000724     C*
010400000724     C                   endif
010500000724     C                   endif
010600000616     C*
010700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010800000616     C                   movel(p)  CMD(2)        depcmd
010900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011000000616     C*
011100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011200011113     C                   Z-ADD     150           LENGH            15 5
011300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011400000616     C                   PARM                    depcmd
011500000616     C                   PARM                    LENGH
011600000616     C*
011700090218     C                   if        not %open(titv1m9ps)
011800090218     C                   open      titv1m9ps
011900000616     C                   except    testdett
012000000613     C                   endif
012100000613     C*
012200070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
012300000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
012400000613     C*
012500010202     C* Effettuo la chiamata al CLLE preposto
012600050128     C                   call(e)   'TITVVTC'
012700010202     C                   parm                    parccm
012800010202     C                   parm                    parmbr
012900010202     C                   parm      '2'           paropz
013000070730     C*
013100070730     C* Effettuo lancio TISI95 solo x chiusura
013200070730     C                   CLEAR                   TISI95DS
013300070730     C                   EVAL      I95TLA = 'C'
013400070730     C                   CALL      'TISI95R'
013500070730     C                   PARM                    TISI95DS
013600010202     C*
013700090218     C                   if        %open(titv1m9ps)
013800000616     C                   except    findett
013900090218     C                   close     titv1m9ps
014000000613     C                   endif
014100000616     C*
014200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
014300011113     C                   Z-ADD     150           LENGH            15 5
014400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
014500000616     C                   PARM                    CMD(3)
014600000616     C                   PARM                    LENGH
014700000616     C*
014800000801     C
014900010201     C                   seton                                        LR
015000000613
015100000613     C*--------------------------------------------------------
015200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
015300000613     C*--------------------------------------------------------
015400000621     C     STPR          BEGSR
015500000613     C*
015600090218     C                   if        not %open(titv1m9p)
015700090218     C                   open      titv1m9p
015800990915     C                   endif
015900990915     C*
016000990915     C                   except    riepilogo
016100990915     C*
016200090218     C                   if        %open(titv1m9p)
016300090218     C                   close     titv1m9p
016400990914     C                   endif
016500990910     C*
016600000613     C                   ENDSR
016700000613     C***
016800990908
016900000801
017000910830     C*--------------------------------------------------------
017100070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
017200910830     C*--------------------------------------------------------
017300070530     C     RWFILE        BEGSR
017400990910     C*
017500990914     C                   if        not %open(tivin00r)
017600990908     C                   open      tivin00r
017700990914     C                   endif
017800021113     C                   if        not %open(fivabwwr)
017900021113     C                   open      fivabwwr
018000990914     C                   endif
018100070530     C*
018200021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
018300020305     C                   exsr      prevat
018400010201     C*
018500010202     C                   if        chkcall = '0'
018600010202     C*
018700021113     C                   if        not %open(fivatwwr)
018800021113     C                   open      fivatwwr
018900010201     C                   endif
019000080117     C*
019100080117     C                   EXSR      INZVAR
019200080117     C                   EXSR      DEFCAM
019300990910     C*
019400010201     C                   clear                   §CTROKVB          5 0
019500020305     C                   clear                   §CTROKVT          5 0
019600000801     C                   clear                   §CTRMO            5 0
019700000801     C                   clear                   §CTRNO            5 0
019800990910     C*
019900020725     C/EXEC SQL
020000020725     C+ declare C1 cursor for select
020100070928     C+ vindta, vinflg, substr(vindta, 315, 16) as sped, rrn(tivin00r)
020200060223     C+ from tivin00r
020300060223     C+ order by sped
020400020725     C+ for read only
020500020725     C/END-EXEC
020600020725     C
020700020725     C/EXEC SQL
020800020725     C+ open C1
020900020725     C/END-EXEC
021000020725     C
021100020725     C/EXEC SQL
021200070530     C+ Fetch C1 into :INPUT_DS
021300020725     C/END-EXEC
021400020725     C*
021500020725     C                   dow       sqlcod = *zeros
021600990913     C*
021700020725     C                   if        vindta > *blanks
021800000613     C                   add       1             rrnum
021900000801     C*
022000020725     C                   if        vinflg = *blanks
022100020725     C                             or vinflg = '0'
022200020725     C                             or vinflg = '2'
022300000801     C*
022400020725     C                   clear                   x_vinmsg
022500020725     C                   eval      x_vinflg = '1'
022600010305     C*
022700010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
022800070928     C                   EVAL      PiStr=%trim(%subst(vindta:315:16))
022900020305     C                   MOVEL(p)  PiStr         curspe
023000010305     C*
023100071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
023200080923     C                   if        *in50 = *off
023300071003     C                   exsr      impvab
023400071003     C                   exsr      wrivab
023500071003     C                   exsr      wrivat_b                                     => carico VAT
023600071003     C                   else
023700080923     C*
023800071009     C                   if        wDISK = 'D'
023900071009     C                   exsr      impvab
024000071009     C                   exsr      wrivab
024100071009     C                   exsr      wrivat_b                                     => carico VAT
024200071009     C                   exsr      wrivat_e                                     => carico VAT
024300071009     C                   else
024400071009     C*
024500010305     C                   if        depspe = *blanks                             => 1° giro
024600010305     C                   eval      depspe = curspe                              => memorizz. spediz
024700080117     C                   clear                   tivinds
024800020305     C                   exsr      impvab
024900071003     C                   exsr      wrivat_b                                     => carico VAT
025000071003     C   50              exsr      wrivat_e                                     => carico VAT
025100010305     C                   else
025200020725     C                   if        depspe <> curspe                             => rottura di spediz
025300010305     C                   eval      depspe = curspe                              => memorizz. spediz
025400070928     C                   exsr      wrivab
025500080117     C                   clear                   tivinds
025600020305     C                   exsr      impvab
025700071003     C                   exsr      wrivat_b                                     => carico VAT
025800071003     C   50              exsr      wrivat_e                                     => carico VAT
025900020305     C                   else                                                   => x stessa spediz
026000020305     C                   exsr      impvab
026100071003     C                   exsr      wrivat_b                                     => carico VAT
026200071003     C   50              exsr      wrivat_e                                     => carico VAT
026300010305     C                   endif
026400010305     C                   endif
026500010305     C                   endif
026600071003     C                   endif
026700071009     C                   endif
026800000905     C*
026900000905     C                   else
027000020725     C                   eval      x_vinflg = '1'
027100000905     C                   endif
027200000905     C*
027300020725     C     VINRRN        chain     tivin000
027400020725     C                   if        %found(tivin00r)
027500020725     C                   eval      y_vinflg = x_vinflg
027600020725     C                   eval      y_vinmsg = x_vinmsg
027700020725     C                   update    tivin000
027800020725     C                   endif
027900020725     C*
028000020725     C/EXEC SQL
028100070530     C+ Fetch C1 into :INPUT_DS
028200020725     C/END-EXEC
028300020725     C*
028400020725     C                   enddo
028500020725     C*
028600020725     C/EXEC SQL
028700020725     C+ close C1
028800020725     C/END-EXEC
028900000905     C*
029000020305     C* Scarico i VAB rimasti "in sospeso"
029100071009     C                   if        wDISK = 'C'
029200071009     C                   exsr      wrivab
029300071009     C                   endif
029400010202     C*
029500010202     C                   endif
029600990910
029700990910     C* Se non ci sono record con errori ...
029800000710     C                   if        §ctrno = 0
029900990910     C* ... restituisco esito OK.
030000990921     C                   eval      wrkesito = '0'
030100990910     C                   else
030200010201     C                   if        §ctrokvb > 0
030300990921     C                   eval      wrkesito = '1'
030400000710     C                   else
030500000710     C                   eval      wrkesito = '2'
030600990910     C                   endif
030700000710     C                   endif
030800990910     C*
030900990914     C                   if        %open(tivin00r)
031000990908     C                   close     tivin00r
031100990914     C                   endif
031200021113     C                   if        %open(fivabwwr)
031300021113     C                   close     fivabwwr
031400990914     C                   endif
031500021113     C                   if        %open(fivatwwr)
031600021113     C                   close     fivatwwr
031700010201     C                   endif
031800990910     C*
031900010201     C                   if        §ctrokvb > 0
032000000724     C                             and vlrpoi <> *zeros
032100010202     C                   exsr      invio
032200990920     C                   endif
032300990920     C*
032400910830     C                   ENDSR
032500000613     C***
032600010305
032700010305     C*----------------------------------------------------*
032800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
032900010305     C*----------------------------------------------------*
033000020305     C     WRIVAB        BEGSR
033100070730     C*
033200071003     C* Considerazioni finali
033300071003     C                   if        VABRMA = *blanks
033400071003     C                   movel(P)  VABRMN        VABRMA
033500071003     C                   endif
033600071003     C*
033700021113     C                   write     fivab000                                     => scarico il VAB
033800010305     C*
033900010305     C                   ENDSR
034000990920
034100000801     C*----------------------------------------------------*
034200000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
034300000801     C*----------------------------------------------------*
034400010201     C     INZVAR        BEGSR
034500000801     C*
034600010201     C                   Z-ADD     *zeros        Num5_0
034700020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
034800020725     C                   MOVEL     '0'           FlgCAS            1
034900000801     C*
035000000801     C                   ENDSR
035100000801     C*----------------------------------------------------*
035200000801     C*  IMPOSTAZIONE CAMPI COSTANTI
035300000801     C*----------------------------------------------------*
035400020904     C     DEFCAM        BEGSR
035500080923     C*
035600090310     C                   SETOFF                                       50
035700000801     C*
035800021113     C                   CLEAR                   FIVAB000
035900021113     C                   CLEAR                   FIVAT000
036000070730     C* Imposto i valori di default...
036100090218     C                   EVAL      VABCCM = 0961218
036200090218     C                   EVAL      VATCCM = 0961218
036300090218     C                   EVAL      VABLNP = 096
036400090218     C                   EVAL      VATLNP = 096
036500070928     C                   EVAL      VABCTR = 000
036600070730     C                   EVAL      VABCBO = '1'
036700090218     C                   EVAL      VABTSP = 'D'
036800090310     C                   EVAL      VABCTM = '7Q'
036900090218     C                   EVAL      VABNAS = 'ABBIGLIAMENTO'
037000070222     C* ... e poi verifico se sono stati passati come parametri
037100070222     C                   IF        vlrppt > *blanks
037200070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
037300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
037400070222     C                   EXSR      CHKNUM
037500070222     C                   IF        PiInt=*on
037600070222     C                   Z-ADD     PiVal         VABCCM
037700070222     C                   Z-ADD     PiVal         VATCCM
037800070222     C                   ENDIF
037900070222     C                   ENDIF
038000070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
038100070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
038200070222     C                   EXSR      CHKNUM
038300070222     C                   IF        PiInt=*on
038400070222     C                   Z-ADD     PiVal         VABLNP
038500070222     C                   Z-ADD     PiVal         VATLNP
038600070222     C                   ENDIF
038700070222     C                   ENDIF
038800070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
038900070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
039000070222     C                   EXSR      CHKNUM
039100070222     C                   IF        PiInt=*on
039200070222     C                   Z-ADD     PiVal         VABCTR
039300070222     C                   ENDIF
039400070928     C                   ENDIF
039500071009     C                   MOVEL     *blanks       wDISK             1
039600071009     C                   EVAL      wDISK = %subst(vlrppt:14:1)
039700071009     C                   IF        wDISK <> *blanks
039800070928     C                   SETON                                        50
039900070222     C                   ENDIF
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
042100090218     C                   EVAL      VABRSD=%trim(%subst(vindta:17:35))
042200070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
042300070928     C     '@':'A'       XLATE     VABRSD        VABRSD
042400070928     C* ==
042500090218     C                   EVAL      VABRD2=%trim(%subst(vindta:52:5))
042600070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
042700070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
042800070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
042900090218     C                   EVAL      VABRMA=%trim(%subst(vindta:187:10))
043000070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
043100080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
043200090218     C                   EVAL      VABNOT=%trim(%subst(vindta:197:35))
043300090218     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
043400080923     C                   IF        %trim(%subst(vindta:390:1)) = 'F'
043500080226     C                   EVAL      VABCBO='1'
043600080226     C                   ENDIF
043700080923     C                   IF        %trim(%subst(vindta:390:1)) = 'A'
043800080226     C                   EVAL      VABCBO='2'
043900080226     C                   ENDIF
044000100208     C                   IF        %trim(%subst(vindta:12:1)) = 'E'
044100080226     C                   EVAL      VABTSP='E'
044200080226     C                   ENDIF
044300100208     C                   IF        %trim(%subst(vindta:12:1)) = 'N'
044400100208     C                   EVAL      VABTSP='C'
044500100208     C                   ENDIF
044600080226     C                   IF        %trim(%subst(vindta:16:1)) = '1'
044700080226     C                   EVAL      VABFFD='S'
044800080226     C                   ENDIF
044900081113     C                   IF        %trim(%subst(vindta:16:1)) = '2'
045000080328     C                   EVAL      VABTC1='A'
045100080226     C                   ENDIF
045200071003     C* CAD
045300070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
045400070928     C                   EXSR      CHKNUM
045500070928     C                   IF        PiInt=*on
045600070928     C                   Z-ADD     PiVal         Num5_0
045700070928     C                   MOVEL     Num5_0        VABCAD
045800070928     C                   ELSE
045900070928     C                   SETON                                        32
046000070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
046100070928     C                             + ' ' + 'VABCAD'
046200070928     C                   ENDIF
046300080923     C* RMN
046400080923     C                   EVAL      PiStr=%trim(%subst(vindta:186+1:11-1))
046500080923     C                   EXSR      CHKNUM
046600080923     C                   IF        PiInt=*on
046700080923     C                   Z-ADD     PiVal         VABRMN
046800080923     C                   ELSE
046900080923     C                   SETON                                        32
047000080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047100080923     C                             + ' ' + 'VABRMN'
047200080923     C                   ENDIF
047300090310     C* NSP
047400080923     C                   EVAL      PiStr=%trim(%subst(vindta:186+1:11-1))
047500080923     C                   EXSR      CHKNUM
047600080923     C                   IF        PiInt=*on
047700080923     C                   Z-ADD     PiVal         VABNSP
047800080923     C                   Z-ADD     PiVal         VATNSP
047900080923     C                   ELSE
048000080923     C                   SETON                                        31
048100080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048200080923     C                             + ' ' + 'VABNSP VATNSP'
048300080923     C                   ENDIF
048400071003     C* NCL
048500070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
048600070928     C                   EXSR      CHKNUM
048700070928     C                   IF        PiInt=*on
048800070928     C                   Z-ADD     PiVal         VABNCL
048900070928     C                   ELSE
049000070928     C                   SETON                                        32
049100070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049200070928     C                             + ' ' + 'VABNCL'
049300070928     C                   ENDIF
049400071003     C* PKB
049500070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
049600070928     C                   EXSR      CHKNUM
049700070928     C                   IF        PiNum=*on
049800070928     C                   Z-ADD     PiVal         VABPKB
049900070928     C                   ELSE
050000070928     C                   SETON                                        32
050100070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
050200070928     C                             + ' ' + 'VABPKB'
050300070928     C                   ENDIF
050400080923     C* CAS
050500080923     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
050600080923     C                             AND
050700080923     C                             %trim(%subst(vindta:280:12))<>*zeros
050800080923     C                             AND
050900080923     C                             %trim(%subst(vindta:280:12))<>'0,00'
051000080923     C                             AND
051100080923     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
051200080923     C                             AND
051300080923     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
051400070928     C                   MOVEL     '1'           FlgCAS
051500090223     C                   EVAL      VABTIC = 'BM'
051600070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
051700070928     C                   EXSR      CHKNUM
051800070928     C                   IF        PiNum=*on
051900070928     C                   Z-ADD     PiVal         VABCAS
052000070928     C                   ELSE
052100070928     C                   SETON                                        32
052200070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052300070928     C                             + ' ' + 'VABCAS'
052400070928     C                   ENDIF
052500070928     C                   ENDIF
052600071003     C* IAS
052700080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
052800080923     C                             AND
052900080923     C                             %trim(%subst(vindta:295:12))<>*zeros
053000080923     C                             AND
053100080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
053200080923     C                             AND
053300080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
053400080923     C                             AND
053500080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
053600080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
053700080923     C                   EXSR      CHKNUM
053800080923     C                   IF        PiNum=*on
053900080923     C                   Z-ADD     PiVal         VABIAS
054000080923     C                   ELSE
054100080923     C                   SETON                                        32
054200080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054300080923     C                             + ' ' + 'VABIAS'
054400080923     C                   ENDIF
054500071003     C                   ENDIF
054600070730     C*
054700070730     C* Considerazioni finali su CBO/CAS
054800070730     C                   IF        FlgCAS = '1'
054900070730     C                   IF        VABCBO = '1'
055000070730     C                   EVAL      VABCBO = '4'
055100070730     C                   ENDIF
055200070730     C                   IF        VABCBO = '2'
055300070730     C                   EVAL      VABCBO = '6'
055400070730     C                   ENDIF
055500070730     C                   ENDIF
055600020305     C*
055700011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
055800011113     C                   EXSR      CHKIMPDIV
055900010202     C*
056000000801     C* Ebbene...
056100000801     C                   ADD       1             §CTRMO
056200070530     C                   IF        *in31 <> *zeros OR
056300070530     C                             *in32 <> *zeros
056400000801     C                   ADD       1             §CTRNO
056500000801     C                   EVAL      recko = vindta
056600000801     C                   EXCEPT    rigadett
056700020725     C                   EVAL      x_vinflg = '2'
056800000801     C                   ELSE
056900010201     C                   ADD       1             §CTROKVB
057000000801     C                   ENDIF
057100000801     C*
057200000801     C                   ENDSR
057300010201     C*----------------------------------------------------*
057400071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
057500010201     C*----------------------------------------------------*
057600071003     C     WRIVAT_B      BEGSR
057700010201     C*
057800021113     C* Valorizzo l buffer di scrittura del FIVAT
057900070928     C* - TIPO RECORD "B"
058000070928     C                   if        %subst(vindta:92:15) <> *blanks
058100070928     C                   eval      VATTRC = 'B'
058200070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
058300021113     C                   write     FIVAT000
058400020725     C                   endif
058500010201     C*
058600010201     C                   ENDSR
058700071003     C*----------------------------------------------------*
058800071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
058900071003     C*----------------------------------------------------*
059000071003     C     WRIVAT_E      BEGSR
059100071003     C*
059200071003     C* Valorizzo l buffer di scrittura del FIVAT
059300071003     C* - TIPO RECORD "E"
059400071003     C                   if        %subst(vindta:331:26) <> *blanks
059500071003     C                   eval      VATTRC = 'E'
059600071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
059700071003     C                   write     FIVAT000
059800071003     C                   endif
059900071003     C*
060000071003     C                   ENDSR
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
063200081113     C                   EVAL      PiDecChr = '.'
063300081113     C                   ENDIF
063400081113     C*
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
079800000830     C                   time                    wn14             14 0
079900000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
080000000830     C                   z-add     wn8           g08dat
080100000830     C                   z-add     *zeros        g08inv
080200000830     C                   movel     '0'           g08err
080300000830     C                   call      'XSRDA8'
080400000830     C                   parm                    wlbda8
080500000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
080600000830     C*
080700000613     C                   ENDSR
080800000613     C***
080900990908
081000090218     Otitv1m9p  E            riepilogo         2
081100990915     O                                              'Upload via Internet'
081200990915     O                                           +1 'Traduzione TIVIN00R -'
081300021113     O                                           +1 'FIVABWWR/FIVATWWR'
081400010201     O                                           +1 'Report testate bolle'
081500990915     O          E            riepilogo   2
081600990915     O                       wrkdata
081700990915     O                       wrkora              +1
081800990915     O                       procname            +1
081900990915     O          E            riepilogo   2
082000990915     O                                              'Cliente..................:'
082100990915     O                       VABCCM        z     +1
082200990915     O          E            riepilogo   2
082300990920     O                                              'Riferimento Strategi.....:'
082400990920     O                       vlrhdl              +1
082500990915     O          E            riepilogo   2
082600990915     O                                              'Giusti...................:'
082700010201     O                       §CTROKVB      2   +  1
082800990915     O          E            riepilogo   2
082900990915     O                                              'Sbagliati e corretti.....:'
083000971022     O                       §CTRMO        2   +  1
083100990915     O          E            riepilogo   2
083200990915     O                                              'Sbagliati e scartati.....:'
083300971022     O                       §CTRNO        2   +  1
083400000613
083500090218     Otitv1m9ps E            testdett          2
083600000613     O                                              'Upload via Internet'
083700000613     O                                           +1 'Traduzione TIVIN00R -'
083800021113     O                                           +1 'FIVABWWR/FIVATWWR'
083900010201     O                                           +1 'Report testate bolle'
084000000616     O          E            testdett    3
084100000613     O                                           +2 'N° rec'
084200000613     O                                           +3 'Anteprima contenuto'
084300000616     O          E            rigadett    2
084400000613     O                       rrnum               +2
084500000621     O                       recko               +3
084600000616     O          E            findett     2
084700000613     O                       wrkdata
084800000613     O                       wrkora              +1
084900000613     O                       procname            +1
085000000616     O          E            findett     2
085100000613     O                                              'Cliente..................:'
085200000613     O                       VABCCM        z     +1
085300000616     O          E            findett     2
085400000613     O                                              'Riferimento Strategi.....:'
085500000613     O                       vlrhdl              +1
085600000616     O          E            findett     2
085700000613     O                                              'Giusti...................:'
085800010201     O                       §CTROKVB      2   +  1
085900000616     O          E            findett     2
086000000613     O                                              'Sbagliati e corretti.....:'
086100000613     O                       §CTRMO        2   +  1
086200000616     O          E            findett     2
086300000613     O                                              'Sbagliati e scartati.....:'
086400000613     O                       §CTRNO        2   +  1
086500000512** CMD - COMANDI CL
086600090218OVRPRTF FILE(TITV1M9P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
086700090218OVRPRTF FILE(TITV1M9PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
086800090218DLTOVR FILE(TITV1M9P TITV1M9PS) LVL(*)
086900000512
087000000512
