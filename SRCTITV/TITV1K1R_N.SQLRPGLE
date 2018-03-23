000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200081113     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800081113     Ftitv1k1p  O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100081113     Ftitv1k1ps O    f  198        PRINTER usropn
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
006400080923     D*------------------
006500080923     D* DS REPERIMENTO NUMERATORE
006600080923     D*------------------
006700080923     D trul33ds      e ds                  inz
006800080923     D*------------------
006900080923     D* DS ARCHITETTURA
007000080923     D*------------------
007100080923     D kpjba         e ds                  inz
007200080923     D*------------------
007300081113
007400081113     D*------------------
007500081113     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
007600081113     D*------------------
007700081113     D PiStr           S            100A   INZ
007800081113     D PiDecChr        S              1A   INZ
007900081113     D PiVal           S             63S30 INZ
008000081113     D PiInt           S               N   INZ
008100081113     D PiNum           S               N   INZ
008200081113
008300081113     D*------------------
008400081113     D* LINKING A DEFINIZIONI ESTERNE
008500081113     D*------------------
008600081113     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
008700081113
008800990908
008900010201
009000010201
009100080117     C*
009200080117     C* Definisco le opzioni con cui verranno d seguito utilizzate le istruzioni SQL
009300080117     C
009400080117     C/EXEC SQL
009500080117     C+ SET OPTION DYNUSRPRF = *OWNER, CLOSQLCSR = *ENDMOD
009600080117     C/END-EXEC
009700080117     C
009800990915     C                   time                    wrkdata
009900990915     C                   time                    wrkora
010000000913     C                   reset                   rrnum
010100990921     C                   reset                   esito
010200990921     C                   reset                   wrkesito
010300000724     C*
010400000724     C* SE OCCORRE SPEDIRE IN FILIALE
010500000724     C                   if        vlrpoi <> *zeros
010600000724     C*
010700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010900000724     C     vlrpoi        chain     azorg01l
011000000724     C                   if        %found
011100000616     C                   movel(p)  CMD(1)        depcmd
011200020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011300000616     C*
011400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011500011113     C                   Z-ADD     150           LENGH            15 5
011600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011700000616     C                   PARM                    depcmd
011800000616     C                   PARM                    LENGH
011900000724     C*
012000000724     C                   endif
012100000724     C                   endif
012200000616     C*
012300000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012400000616     C                   movel(p)  CMD(2)        depcmd
012500000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012600000616     C*
012700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012800011113     C                   Z-ADD     150           LENGH            15 5
012900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013000000616     C                   PARM                    depcmd
013100000616     C                   PARM                    LENGH
013200000616     C*
013300081113     C                   if        not %open(titv1k1ps)
013400081113     C                   open      titv1k1ps
013500000616     C                   except    testdett
013600000613     C                   endif
013700000613     C*
013800070530     C                   EXSR      RWFILE                                       LETT/SCR. VAB/VAT
013900000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014000000613     C*
014100010202     C* Effettuo la chiamata al CLLE preposto
014200050128     C                   call(e)   'TITVVTC'
014300010202     C                   parm                    parccm
014400010202     C                   parm                    parmbr
014500010202     C                   parm      '2'           paropz
014600070730     C*
014700070730     C* Effettuo lancio TISI95 solo x chiusura
014800070730     C                   CLEAR                   TISI95DS
014900070730     C                   EVAL      I95TLA = 'C'
015000070730     C                   CALL      'TISI95R'
015100070730     C                   PARM                    TISI95DS
015200010202     C*
015300081113     C                   if        %open(titv1k1ps)
015400000616     C                   except    findett
015500081113     C                   close     titv1k1ps
015600000613     C                   endif
015700000616     C*
015800000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015900011113     C                   Z-ADD     150           LENGH            15 5
016000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
016100000616     C                   PARM                    CMD(3)
016200000616     C                   PARM                    LENGH
016300000616     C*
016400000801     C
016500010201     C                   seton                                        LR
016600000613
016700000613     C*--------------------------------------------------------
016800000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016900000613     C*--------------------------------------------------------
017000000621     C     STPR          BEGSR
017100000613     C*
017200081113     C                   if        not %open(titv1k1p)
017300081113     C                   open      titv1k1p
017400990915     C                   endif
017500990915     C*
017600990915     C                   except    riepilogo
017700990915     C*
017800081113     C                   if        %open(titv1k1p)
017900081113     C                   close     titv1k1p
018000990914     C                   endif
018100990910     C*
018200000613     C                   ENDSR
018300000613     C***
018400990908
018500000801
018600910830     C*--------------------------------------------------------
018700070530     C* RWFILE   LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
018800910830     C*--------------------------------------------------------
018900070530     C     RWFILE        BEGSR
019000990910     C*
019100990914     C                   if        not %open(tivin00r)
019200990908     C                   open      tivin00r
019300990914     C                   endif
019400021113     C                   if        not %open(fivabwwr)
019500021113     C                   open      fivabwwr
019600990914     C                   endif
019700070530     C*
019800021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
019900020305     C                   exsr      prevat
020000010201     C*
020100010202     C                   if        chkcall = '0'
020200010202     C*
020300021113     C                   if        not %open(fivatwwr)
020400021113     C                   open      fivatwwr
020500010201     C                   endif
020600080117     C*
020700080117     C                   EXSR      INZVAR
020800080117     C                   EXSR      DEFCAM
020900990910     C*
021000010201     C                   clear                   §CTROKVB          5 0
021100020305     C                   clear                   §CTROKVT          5 0
021200000801     C                   clear                   §CTRMO            5 0
021300000801     C                   clear                   §CTRNO            5 0
021400990910     C*
021500020725     C/EXEC SQL
021600020725     C+ declare C1 cursor for select
021700070928     C+ vindta, vinflg, substr(vindta, 315, 16) as sped, rrn(tivin00r)
021800060223     C+ from tivin00r
021900060223     C+ order by sped
022000020725     C+ for read only
022100020725     C/END-EXEC
022200020725     C
022300020725     C/EXEC SQL
022400020725     C+ open C1
022500020725     C/END-EXEC
022600020725     C
022700020725     C/EXEC SQL
022800070530     C+ Fetch C1 into :INPUT_DS
022900020725     C/END-EXEC
023000020725     C*
023100020725     C                   dow       sqlcod = *zeros
023200990913     C*
023300020725     C                   if        vindta > *blanks
023400000613     C                   add       1             rrnum
023500000801     C*
023600020725     C                   if        vinflg = *blanks
023700020725     C                             or vinflg = '0'
023800020725     C                             or vinflg = '2'
023900000801     C*
024000020725     C                   clear                   x_vinmsg
024100020725     C                   eval      x_vinflg = '1'
024200010305     C*
024300010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
024400070928     C                   EVAL      PiStr=%trim(%subst(vindta:315:16))
024500020305     C                   MOVEL(p)  PiStr         curspe
024600010305     C*
024700071003     C* Se trattasi d soluzione Disk C => gestisco altrimenti Disk A
024800080923     C                   if        *in50 = *off
024900071003     C                   exsr      impvab
025000071003     C                   exsr      wrivab
025100071003     C                   exsr      wrivat_b                                     => carico VAT
025200071003     C                   else
025300080923     C*
025400071009     C                   if        wDISK = 'D'
025500071009     C                   exsr      impvab
025600071009     C                   exsr      wrivab
025700071009     C                   exsr      wrivat_b                                     => carico VAT
025800071009     C                   exsr      wrivat_e                                     => carico VAT
025900071009     C                   else
026000071009     C*
026100010305     C                   if        depspe = *blanks                             => 1° giro
026200010305     C                   eval      depspe = curspe                              => memorizz. spediz
026300080117     C                   clear                   tivinds
026400020305     C                   exsr      impvab
026500071003     C                   exsr      wrivat_b                                     => carico VAT
026600071003     C   50              exsr      wrivat_e                                     => carico VAT
026700010305     C                   else
026800020725     C                   if        depspe <> curspe                             => rottura di spediz
026900010305     C                   eval      depspe = curspe                              => memorizz. spediz
027000070928     C                   exsr      wrivab
027100080117     C                   clear                   tivinds
027200020305     C                   exsr      impvab
027300071003     C                   exsr      wrivat_b                                     => carico VAT
027400071003     C   50              exsr      wrivat_e                                     => carico VAT
027500020305     C                   else                                                   => x stessa spediz
027600020305     C                   exsr      impvab
027700071003     C                   exsr      wrivat_b                                     => carico VAT
027800071003     C   50              exsr      wrivat_e                                     => carico VAT
027900010305     C                   endif
028000010305     C                   endif
028100010305     C                   endif
028200071003     C                   endif
028300071009     C                   endif
028400000905     C*
028500000905     C                   else
028600020725     C                   eval      x_vinflg = '1'
028700000905     C                   endif
028800000905     C*
028900020725     C     VINRRN        chain     tivin000
029000020725     C                   if        %found(tivin00r)
029100020725     C                   eval      y_vinflg = x_vinflg
029200020725     C                   eval      y_vinmsg = x_vinmsg
029300020725     C                   update    tivin000
029400020725     C                   endif
029500020725     C*
029600020725     C/EXEC SQL
029700070530     C+ Fetch C1 into :INPUT_DS
029800020725     C/END-EXEC
029900020725     C*
030000020725     C                   enddo
030100020725     C*
030200020725     C/EXEC SQL
030300020725     C+ close C1
030400020725     C/END-EXEC
030500000905     C*
030600020305     C* Scarico i VAB rimasti "in sospeso"
030700071009     C                   if        wDISK = 'C'
030800071009     C                   exsr      wrivab
030900071009     C                   endif
031000010202     C*
031100010202     C                   endif
031200990910
031300990910     C* Se non ci sono record con errori ...
031400000710     C                   if        §ctrno = 0
031500990910     C* ... restituisco esito OK.
031600990921     C                   eval      wrkesito = '0'
031700990910     C                   else
031800010201     C                   if        §ctrokvb > 0
031900990921     C                   eval      wrkesito = '1'
032000000710     C                   else
032100000710     C                   eval      wrkesito = '2'
032200990910     C                   endif
032300000710     C                   endif
032400990910     C*
032500990914     C                   if        %open(tivin00r)
032600990908     C                   close     tivin00r
032700990914     C                   endif
032800021113     C                   if        %open(fivabwwr)
032900021113     C                   close     fivabwwr
033000990914     C                   endif
033100021113     C                   if        %open(fivatwwr)
033200021113     C                   close     fivatwwr
033300010201     C                   endif
033400990910     C*
033500010201     C                   if        §ctrokvb > 0
033600000724     C                             and vlrpoi <> *zeros
033700010202     C                   exsr      invio
033800990920     C                   endif
033900990920     C*
034000910830     C                   ENDSR
034100000613     C***
034200010305
034300010305     C*----------------------------------------------------*
034400020305     C*  SCARICAMENTO BUFFER RECORDS VAB
034500010305     C*----------------------------------------------------*
034600020305     C     WRIVAB        BEGSR
034700070730     C*
034800071003     C* Considerazioni finali
034900071003     C                   if        VABRMA = *blanks
035000071003     C                   movel(P)  VABRMN        VABRMA
035100071003     C                   endif
035200071003     C*
035300021113     C                   write     fivab000                                     => scarico il VAB
035400010305     C*
035500010305     C                   ENDSR
035600990920
035700000801     C*----------------------------------------------------*
035800000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
035900000801     C*----------------------------------------------------*
036000010201     C     INZVAR        BEGSR
036100000801     C*
036200010201     C                   Z-ADD     *zeros        Num5_0
036300020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
036400020725     C                   MOVEL     '0'           FlgCAS            1
036500000801     C*
036600000801     C                   ENDSR
036700000801     C*----------------------------------------------------*
036800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
036900000801     C*----------------------------------------------------*
037000020904     C     DEFCAM        BEGSR
037100080923     C*
037200080923     C                   SETOFF                                       5051
037300000801     C*
037400021113     C                   CLEAR                   FIVAB000
037500021113     C                   CLEAR                   FIVAT000
037600070730     C* Imposto i valori di default...
037700081113     C                   EVAL      VABCCM = 0593668
037800081113     C                   EVAL      VATCCM = 0593668
037900081113     C                   EVAL      VABLNP = 059
038000081113     C                   EVAL      VATLNP = 059
038100070928     C                   EVAL      VABCTR = 000
038200070730     C                   EVAL      VABCBO = '1'
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
041300080923     C                   IF        %subst(vlrppt:15:1) = 'S'
041400080923     C                   SETON                                        51
041500080923     C                   ENDIF
041600070222     C                   ENDIF
041700071009     C*
041800071009     C   50              EVAL      VABCTM = '7Q'
041900000801     C*
042000000801     C                   ENDSR
042100000801     C*----------------------------------------------------*
042200021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
042300000801     C*----------------------------------------------------*
042400010201     C     IMPVAB        BEGSR
042500070530     C*
042600070530     C                   SETOFF                                       3132
042700070928     C*
042800070928     C                   EXSR      INZVAR
042900070928     C                   EXSR      DEFCAM
043000070928     C*
043100070928     C                   MOVE(P)   vlrpoi        VABFGS
043200070928     C                   MOVE(P)   vlrpoi        VATFGS
043300070928     C*
043400070928     C                   MOVEL     datcor        VABAAS
043500070928     C                   MOVEL     datcor        VATAAS
043600070928     C                   MOVE      datcor        VABMGS
043700070928     C                   EVAL      VABRSD=%trim(%subst(vindta:17:40))
043800070928     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
043900070928     C     '@':'A'       XLATE     VABRSD        VABRSD
044000070928     C* ==
044100070928     C                   EVAL      VABIND=%trim(%subst(vindta:57:35))
044200070928     C                   EVAL      VABLOD=%trim(%subst(vindta:112:30))
044300070928     C                   EVAL      VABPRD=%trim(%subst(vindta:142:2))
044400070928     C                   EVAL      VABRMA=%trim(%subst(vindta:315:16))
044500070928     C                   EVAL      VABVCA=%trim(%subst(vindta:277:3))
044600080226     C                   EVAL      VABVAS=%trim(%subst(vindta:292:3))
044700070928     C                   EVAL      VABNAS=%trim(%subst(vindta:145:20))
044800100517     C*
044900100517     C                   select
045000100517     C                   when      %subst(vindta:197:3) = '*BM'
045100100517     C                   eval      VABTIC = 'BM'
045200100517     C                   eval      VABNOT=%trim(%subst(vindta:197+3:35-3))
045300100517     C                   when      %subst(vindta:197:3) = '*CM'
045400100517     C                   eval      VABTIC = 'CM'
045500100517     C                   eval      VABNOT=%trim(%subst(vindta:197+3:35-3))
045600100517     C                   other
045700100517     C                   eval      VABNOT=%trim(%subst(vindta:197:35))
045800100517     C                   endsl
045900100517     C*
046000070928     C                   EVAL      VABNT2=%trim(%subst(vindta:197+35:80-35))
046100080923     C                   IF        %trim(%subst(vindta:390:1)) = 'F'
046200080226     C                   EVAL      VABCBO='1'
046300080226     C                   ENDIF
046400080923     C                   IF        %trim(%subst(vindta:390:1)) = 'A'
046500080226     C                   EVAL      VABCBO='2'
046600080226     C                   ENDIF
046700080226     C                   IF        %trim(%subst(vindta:12:1)) = 'D'
046800080226     C                   EVAL      VABTSP='E'
046900080226     C                   ENDIF
047000080226     C                   IF        %trim(%subst(vindta:16:1)) = '1'
047100080226     C                   EVAL      VABFFD='S'
047200080226     C                   ENDIF
047300081113     C                   IF        %trim(%subst(vindta:16:1)) = '2'
047400080328     C                   EVAL      VABTC1='A'
047500080226     C                   ENDIF
047600071003     C* CAD
047700070928     C                   EVAL      PiStr=%trim(%subst(vindta:107:5))
047800070928     C                   EXSR      CHKNUM
047900070928     C                   IF        PiInt=*on
048000070928     C                   Z-ADD     PiVal         Num5_0
048100070928     C                   MOVEL     Num5_0        VABCAD
048200070928     C                   ELSE
048300070928     C                   SETON                                        32
048400070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048500070928     C                             + ' ' + 'VABCAD'
048600070928     C                   ENDIF
048700080923     C* RMN
048800110224     C                   EVAL      PiStr=%trim(%subst(vindta:330:6))
048900080923     C                   EXSR      CHKNUM
049000080923     C                   IF        PiInt=*on
049100080923     C                   Z-ADD     PiVal         VABRMN
049200080923     C                   ELSE
049300080923     C                   SETON                                        32
049400080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049500080923     C                             + ' ' + 'VABRMN'
049600080923     C                   ENDIF
049700080923     C* NSP
049800080923     C                   IF        *in51 = *on
049900080923     C* NSP => Stacco un numeratore da AZNUM
050000080923     C                   clear                   TRUL33DS
050100080923     C                   eval      I33OPE = *zeros
050200080923     C                   eval      I33CNU = 302
050300080923     C                   eval      I33NUM = 1
050400080923     C                   movel     TRUL33DS      KPJBU
050500080923     C                   call      'TRUL33R'
050600080923     C                   parm                    KPJBA
050700080923     C                   movel     KPJBU         TRUL33DS
050800080923     C                   if        O33ERR = *zeros
050900080923     C                   z-add     O33NRF        VABNSP
051000080923     C                   z-add     O33NRF        VATNSP
051100080923     C                   else
051200080923     C                   SETON                                        31
051300080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051400080923     C                             + ' ' + 'VABNSP VATNSP'
051500080923     C                   endif
051600080923     C                   ELSE
051700110224     C                   EVAL      PiStr=%trim(%subst(vindta:330:6))
051800080923     C                   EXSR      CHKNUM
051900080923     C                   IF        PiInt=*on
052000080923     C                   Z-ADD     PiVal         VABNSP
052100080923     C                   Z-ADD     PiVal         VATNSP
052200080923     C                   ELSE
052300080923     C                   SETON                                        31
052400080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
052500080923     C                             + ' ' + 'VABNSP VATNSP'
052600080923     C                   ENDIF
052700080923     C                   ENDIF
052800071003     C* NCL
052900070928     C                   EVAL      PiStr=%trim(%subst(vindta:165:5))
053000070928     C                   EXSR      CHKNUM
053100070928     C                   IF        PiInt=*on
053200070928     C                   Z-ADD     PiVal         VABNCL
053300070928     C                   ELSE
053400070928     C                   SETON                                        32
053500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
053600070928     C                             + ' ' + 'VABNCL'
053700070928     C                   ENDIF
053800071003     C* PKB
053900070928     C                   EVAL      PiStr=%trim(%subst(vindta:170:8))
054000070928     C                   EXSR      CHKNUM
054100070928     C                   IF        PiNum=*on
054200070928     C                   Z-ADD     PiVal         VABPKB
054300070928     C                   ELSE
054400070928     C                   SETON                                        32
054500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
054600070928     C                             + ' ' + 'VABPKB'
054700070928     C                   ENDIF
054800080923     C* CAS
054900080923     C                   IF        %trim(%subst(vindta:280:12))<>*blanks
055000080923     C                             AND
055100080923     C                             %trim(%subst(vindta:280:12))<>*zeros
055200080923     C                             AND
055300080923     C                             %trim(%subst(vindta:280:12))<>'0,00'
055400080923     C                             AND
055500080923     C                             %trim(%subst(vindta:280:12))<>'000000000,00'
055600080923     C                             AND
055700080923     C                             %trim(%subst(vindta:280:12))<>'000000000.00'
055800070928     C                   MOVEL     '1'           FlgCAS
055900070928     C                   EVAL      PiStr=%trim(%subst(vindta:280:12))
056000070928     C                   EXSR      CHKNUM
056100070928     C                   IF        PiNum=*on
056200070928     C                   Z-ADD     PiVal         VABCAS
056300070928     C                   ELSE
056400070928     C                   SETON                                        32
056500070928     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
056600070928     C                             + ' ' + 'VABCAS'
056700070928     C                   ENDIF
056800070928     C                   ENDIF
056900071003     C* IAS
057000080923     C                   IF        %trim(%subst(vindta:295:12))<>*blanks
057100080923     C                             AND
057200080923     C                             %trim(%subst(vindta:295:12))<>*zeros
057300080923     C                             AND
057400080923     C                             %trim(%subst(vindta:295:12))<>'0,00'
057500080923     C                             AND
057600080229     C                             %trim(%subst(vindta:295:12))<>'000000000,00'
057700080923     C                             AND
057800080923     C                             %trim(%subst(vindta:295:12))<>'000000000.00'
057900080923     C                   EVAL      PiStr=%trim(%subst(vindta:295:12))
058000080923     C                   EXSR      CHKNUM
058100080923     C                   IF        PiNum=*on
058200080923     C                   Z-ADD     PiVal         VABIAS
058300080923     C                   ELSE
058400080923     C                   SETON                                        32
058500080923     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
058600080923     C                             + ' ' + 'VABIAS'
058700080923     C                   ENDIF
058800071003     C                   ENDIF
058900070730     C*
059000070730     C* Considerazioni finali su CBO/CAS
059100070730     C                   IF        FlgCAS = '1'
059200070730     C                   IF        VABCBO = '1'
059300070730     C                   EVAL      VABCBO = '4'
059400070730     C                   ENDIF
059500070730     C                   IF        VABCBO = '2'
059600070730     C                   EVAL      VABCBO = '6'
059700070730     C                   ENDIF
059800070730     C                   ENDIF
059900020305     C*
060000011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
060100011113     C                   EXSR      CHKIMPDIV
060200010202     C*
060300000801     C* Ebbene...
060400000801     C                   ADD       1             §CTRMO
060500070530     C                   IF        *in31 <> *zeros OR
060600070530     C                             *in32 <> *zeros
060700000801     C                   ADD       1             §CTRNO
060800000801     C                   EVAL      recko = vindta
060900000801     C                   EXCEPT    rigadett
061000020725     C                   EVAL      x_vinflg = '2'
061100000801     C                   ELSE
061200010201     C                   ADD       1             §CTROKVB
061300000801     C                   ENDIF
061400000801     C*
061500000801     C                   ENDSR
061600010201     C*----------------------------------------------------*
061700071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "B"
061800010201     C*----------------------------------------------------*
061900071003     C     WRIVAT_B      BEGSR
062000010201     C*
062100021113     C* Valorizzo l buffer di scrittura del FIVAT
062200070928     C* - TIPO RECORD "B"
062300070928     C                   if        %subst(vindta:92:15) <> *blanks
062400070928     C                   eval      VATTRC = 'B'
062500070928     C                   eval      VATNOT = %trim(%subst(vindta:92:15))
062600021113     C                   write     FIVAT000
062700020725     C                   endif
062800010201     C*
062900010201     C                   ENDSR
063000071003     C*----------------------------------------------------*
063100071003     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT) - TRC "E"
063200071003     C*----------------------------------------------------*
063300071003     C     WRIVAT_E      BEGSR
063400071003     C*
063500071003     C* Valorizzo l buffer di scrittura del FIVAT
063600071003     C* - TIPO RECORD "E"
063700071003     C                   if        %subst(vindta:331:26) <> *blanks
063800071003     C                   eval      VATTRC = 'E'
063900071003     C                   eval      VATNOT = %trim(%subst(vindta:331:26))
064000071003     C                   write     FIVAT000
064100071003     C                   endif
064200071003     C*
064300071003     C                   ENDSR
064400020904
064500020904
064600010202     C*----------------------------------------------------*
064700021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
064800010202     C*----------------------------------------------------*
064900020305     C     PREVAT        BEGSR
065000010202     C*
065100021113     C* Compongo il nome del membro da dare al FIVATWWR
065200010202     C                   eval      parmbr = vlrhdl
065300010202     C                   movel     'M'           parmbr
065400070530     C                   eval      parccm = %subst(vlrKSC:2:7)
065500010202     C                   eval      paropz = '1'
065600010202     C* Effettuo la chiamata al CLLE preposto
065700050128     C                   call(e)   'TITVVTC'
065800010202     C                   parm                    parccm
065900010202     C                   parm                    parmbr
066000010202     C                   parm                    paropz
066100010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
066200010202     C                   if        %error
066300010202     C                   movel     '1'           chkcall
066400010202     C                   else
066500010202     C                   movel     '0'           chkcall
066600010202     C                   endif
066700010202     C*
066800010202     C                   ENDSR
066900000801     C*----------------------------------------------------*
067000000801     C*  CONTROLLO NUMERICITA' CAMPI
067100000801     C*----------------------------------------------------*
067200000801     C     CHKNUM        BEGSR
067300081113     C*
067400081113     C                   IF        PiDecChr = *blanks
067500081113     C                   EVAL      PiDecChr = '.'
067600081113     C                   ENDIF
067700081113     C*
067800081113     C                   callp(e)  UBISNUM_Check(PiStr
067900081113     C                                          :PiDecChr
068000081113     C                                          :PiVal
068100081113     C                                          :PiNum
068200081113     C                                          :PiInt)
068300081113     C*
068400000801     C                   IF        %error
068500000801     C                   EVAL      PiInt=*off
068600000801     C                   ENDIF
068700000801     C*
068800000801     C                   ENDSR
068900000801     C***
069000000801
069100011113
069200011113     C*----------------------------------------------------*
069300011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
069400011113     C*----------------------------------------------------*
069500011113     C     CHKIMPDIV     BEGSR
069600011113     C*
069700011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
069800011113     C                   Z-ADD     *zeros        wrkDec            9 9
069900011113     C*
070000011113     C* Come prima cosa effettuo considerazioni sulla divisa
070100011113     C                   IF        vabIAS > *zeros
070200011113     C                   IF        vabVAS <> 'EUR'
070300011113     C                   EVAL      vabVAS =  'ITL'
070400011113     C                   ENDIF
070500011113     C                   ENDIF
070600011113     C*
070700011113     C                   IF        vabCAS > *zeros
070800011113     C                   IF        vabVCA <> 'EUR'
070900011113     C                   EVAL      vabVCA =  'ITL'
071000011113     C                   ENDIF
071100011113     C                   ENDIF
071200011113     C*
071300011113     C                   IF        vabVMD > *zeros
071400020305     C                   IF        vabVAD <> 'EUR'
071500011113     C                   EVAL      vabVAD =  'ITL'
071600011113     C                   ENDIF
071700011113     C                   ENDIF
071800011113     C*
071900011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
072000011113     C                   Z-ADD     vabIAS        wrkDec
072100011113     C                   IF        wrkDec > *zeros
072200011113     C                   IF        vabVAS = 'ITL'
072300011113     C                   EVAL      vabIAS = *zeros
072400011113     C                   ENDIF
072500011113     C                   ENDIF
072600011113     C*
072700011113     C* Stabilisco se il contrasegno ha decimali valorizzati
072800011113     C                   Z-ADD     vabCAS        wrkDec
072900011113     C                   IF        wrkDec > *zeros
073000011113     C                   IF        vabVCA = 'ITL'
073100011113     C                   EVAL      vabCAS = *zeros
073200011113     C                   ENDIF
073300011113     C                   ENDIF
073400011113     C*
073500011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
073600011113     C                   Z-ADD     vabVMD        wrkDec
073700011113     C                   IF        wrkDec > *zeros
073800011113     C                   IF        vabVAD = 'ITL'
073900011113     C                   EVAL      vabVMD = *zeros
074000011113     C                   ENDIF
074100011113     C                   ENDIF
074200011113     C*
074300011113     C                   ENDSR
074400011113     C***
074500011113
074600011113
074700000801
074800000801
074900990920      /TITLE Invio dei dati al punto operativo.
075000010202     C     invio         BEGSR
075100990920     C*
075200021113     C* 1° invio FIVAT
075300010201     C                   reset                   dscmz
075400010201     C                   move      vlrpoi        cmzdst
075500021113     C                   eval      cmzfld = 'FIVATWWR'
075600010201     C                   eval      cmzmbd = vlrhdl
075700010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075800021009     C***                if        prmfir = *blanks
075900021113     C                   eval      cmzfla = 'FIVAT00F'
076000021113     C                   eval      cmzmba = 'FIVAT00F'
076100021009     C***                else
076200021009     C***                eval      cmzfla = prmfir
076300021009     C***                eval      cmzmba = prmfir
076400021009     C***                endif
076500010201     C                   eval      cmznrr = *zeros
076600020305     C                   move      §ctrokvt      cmznrr
076700021018     C                   eval      cmzlba = vlrfl1
076800010201     C                   call(e)   'TIS711C'
076900010201     C                   parm                    dscmz
077000010201     C                   parm      *blanks       esito
077100010205     C                   if        %error
077200010205     C                             or cmzerr = '1'
077300010205     C                             or esito  = '1'
077400010205     C                   eval      wrkesito = '3'
077500010205     C                   else
077600010201     C*
077700021113     C* 2° invio FIVAB
077800010201     C                   reset                   dscmz
077900010201     C                   move      vlrpoi        cmzdst
078000010201     C                   eval      cmzfld = vlrfou
078100010201     C                   eval      cmzmbd = vlrhdl
078200010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
078300021009     C***                if        prmfir = *blanks
078400021113     C                   eval      cmzfla = 'FIVAB00F'
078500021113     C                   eval      cmzmba = 'FIVAB00F'
078600021009     C***                else
078700021009     C***                eval      cmzfla = prmfir
078800021009     C***                eval      cmzmba = prmfir
078900021009     C***                endif
079000010201     C                   eval      cmznrr = *zeros
079100010201     C                   move      §ctrokvb      cmznrr
079200021018     C                   eval      cmzlba = vlrfl1
079300010201     C                   call(e)   'TIS711C'
079400010201     C                   parm                    dscmz
079500010201     C                   parm      *blanks       esito
079600010201     C                   if        %error
079700010201     C                             or cmzerr = '1'
079800010201     C                             or esito  = '1'
079900010201     C                   eval      wrkesito = '3'
080000010201     C                   endif
080100010205     C                   endif
080200990920     C*
080300000613     C                   ENDSR
080400000613     C***
080500070411
080600070411     C     *pssr         BEGSR
080700070411     C*
080800070411     C                   if        %open(tivin00r)
080900070411     C                   close     tivin00r
081000070411     C                   endif
081100070411     C                   if        %open(fivabwwr)
081200070411     C                   close     fivabwwr
081300070411     C                   endif
081400070411     C                   if        %open(fivatwwr)
081500070411     C                   close     fivatwwr
081600070411     C                   endif
081700070411     C*
081800070411     C* Effettuo la chiamata al CLLE preposto
081900070411     C                   call(e)   'TITVVTC'
082000070411     C                   parm                    parccm
082100070411     C                   parm                    parmbr
082200070411     C                   parm      '2'           paropz
082300070411     C*
082400070411     C                   eval      wrkesito = '2'
082500070411     C*
082600070411     C                   seton                                        LR
082700070411     C*
082800070411     C                   ENDSR     '*CANCL'
082900070411     C***
083000070411
083100990910
083200000613     C     *inzsr        BEGSR
083300990910     C*
083400990910     C     *entry        plist
083500990920     C                   parm                    tivlrds
083600990921     C                   parm      wrkesito      esito
083700000724     C                   parm                    prmlit
083800000710     C                   parm                    prmfir
083900000613     C*
084000000830     C* CALCOLA LA DATA CORRENTE
084100000830     C                   time                    wn14             14 0
084200000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
084300000830     C                   z-add     wn8           g08dat
084400000830     C                   z-add     *zeros        g08inv
084500000830     C                   movel     '0'           g08err
084600000830     C                   call      'XSRDA8'
084700000830     C                   parm                    wlbda8
084800000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
084900000830     C*
085000000613     C                   ENDSR
085100000613     C***
085200990908
085300081113     Otitv1k1p  E            riepilogo         2
085400990915     O                                              'Upload via Internet'
085500990915     O                                           +1 'Traduzione TIVIN00R -'
085600021113     O                                           +1 'FIVABWWR/FIVATWWR'
085700010201     O                                           +1 'Report testate bolle'
085800990915     O          E            riepilogo   2
085900990915     O                       wrkdata
086000990915     O                       wrkora              +1
086100990915     O                       procname            +1
086200990915     O          E            riepilogo   2
086300990915     O                                              'Cliente..................:'
086400990915     O                       VABCCM        z     +1
086500990915     O          E            riepilogo   2
086600990920     O                                              'Riferimento Strategi.....:'
086700990920     O                       vlrhdl              +1
086800990915     O          E            riepilogo   2
086900990915     O                                              'Giusti...................:'
087000010201     O                       §CTROKVB      2   +  1
087100990915     O          E            riepilogo   2
087200990915     O                                              'Sbagliati e corretti.....:'
087300971022     O                       §CTRMO        2   +  1
087400990915     O          E            riepilogo   2
087500990915     O                                              'Sbagliati e scartati.....:'
087600971022     O                       §CTRNO        2   +  1
087700000613
087800081113     Otitv1k1ps E            testdett          2
087900000613     O                                              'Upload via Internet'
088000000613     O                                           +1 'Traduzione TIVIN00R -'
088100021113     O                                           +1 'FIVABWWR/FIVATWWR'
088200010201     O                                           +1 'Report testate bolle'
088300000616     O          E            testdett    3
088400000613     O                                           +2 'N° rec'
088500000613     O                                           +3 'Anteprima contenuto'
088600000616     O          E            rigadett    2
088700000613     O                       rrnum               +2
088800000621     O                       recko               +3
088900000616     O          E            findett     2
089000000613     O                       wrkdata
089100000613     O                       wrkora              +1
089200000613     O                       procname            +1
089300000616     O          E            findett     2
089400000613     O                                              'Cliente..................:'
089500000613     O                       VABCCM        z     +1
089600000616     O          E            findett     2
089700000613     O                                              'Riferimento Strategi.....:'
089800000613     O                       vlrhdl              +1
089900000616     O          E            findett     2
090000000613     O                                              'Giusti...................:'
090100010201     O                       §CTROKVB      2   +  1
090200000616     O          E            findett     2
090300000613     O                                              'Sbagliati e corretti.....:'
090400000613     O                       §CTRMO        2   +  1
090500000616     O          E            findett     2
090600000613     O                                              'Sbagliati e scartati.....:'
090700000613     O                       §CTRNO        2   +  1
090800000512** CMD - COMANDI CL
090900081113OVRPRTF FILE(TITV1K1P)  TOFILE(TIS7T7P)  OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
091000081113OVRPRTF FILE(TITV1K1PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL)  FORMTYPE(RICCLI) OUTQ(
091100081113DLTOVR FILE(TITV1K1P TITV1K1PS) LVL(*)
091200000512
091300000512
