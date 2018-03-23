000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400020724     Fazorg01l  iF   e           k DISK
000500020725     Ftivin00r  uF   E             DISK    usropn prefix(y_)
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800020725     Ftitv19p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100020725     Ftitv19ps  O    f  198        PRINTER usropn
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
002600990910     D esito           s              1
002700000724     D prmlit          s             10
002800000710     D prmfir          s             10
002900990921     D wrkesito        s                   like(esito)
003000990915     D wrkdata         s               d
003100990915     D wrkora          s               t
003200000613     D rrnum           s              6  0 INZ(*zeros)
003300000621     D recko           s            150    INZ(*blanks)
003400011113     D depcmd          s            150    INZ(*blanks)
003500110728     D depspe          s              7    INZ(*blanks)
003600010213     D depdat          s              8  0 INZ(*zeros)
003700010213     D depaas          s              4    INZ(*zeros)
003800110728     D curspe          s              7    INZ(*blanks)
003900010202     D parccm          s              8    INZ(*blanks)
004000010202     D parmbr          s             10    INZ(*blanks)
004100010202     D paropz          s              1    INZ(*blanks)
004200010202     D chkcall         s              1    INZ(*blanks)
004300020725     D tivinds       e ds                  extname(tivin00r) prefix(x_)
004400000801
004500010213     D                 DS                  INZ
004600010213     D  aaaa                   1      4  0
004700010213     D  mmgg                   5      8  0
004800010213     D  mm                     5      6  0
004900010213     D  gg                     7      8  0
005000010213     D  datawrk                1      8  0
005100010213
005200000801     D  Num5_0         s              5  0
005300020904     D  SkChkDgt       s              1    DIM(35)
005400000830
005500000830     D*------------------
005600000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
005700000830     D*------------------
005800000830     D WLBDA8          DS                  INZ
005900000830     D  G08DAT                 1      8  0
006000000830     D  G08INV                 9     16  0
006100000830     D  G08ERR                17     17
006200000830     D  G08TGI                18     22  0
006300000830     D*
006400020725
006500020725     D*------------------
006600020725     D* DEFINIZIONE DS DI LETTURA ARCHIVIO DI INPUT VIA SQL
006700020725     D*------------------
006800110728     D PERLAGLPDS      DS                  INZ
006900110728     D  VINDTA                     2048
007000110728     D  VINFLG                        1
007100110728     D  VINSPE                        7
007200110728     D  VINRRN                        8  0
007300020725     D*
007400990908
007500010201
007600010201
007700990915     C                   time                    wrkdata
007800990915     C                   time                    wrkora
007900000913     C                   reset                   rrnum
008000990921     C                   reset                   esito
008100990921     C                   reset                   wrkesito
008200000724     C*
008300000724     C* SE OCCORRE SPEDIRE IN FILIALE
008400000724     C                   if        vlrpoi <> *zeros
008500000724     C*
008600000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
008700000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008800000724     C     vlrpoi        chain     azorg01l
008900000724     C                   if        %found
009000000616     C                   movel(p)  CMD(1)        depcmd
009100020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
009200000616     C*
009300000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
009400011113     C                   Z-ADD     150           LENGH            15 5
009500000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009600000616     C                   PARM                    depcmd
009700000616     C                   PARM                    LENGH
009800000724     C*
009900000724     C                   endif
010000000724     C                   endif
010100000616     C*
010200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010300000616     C                   movel(p)  CMD(2)        depcmd
010400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
010500000616     C*
010600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
010700011113     C                   Z-ADD     150           LENGH            15 5
010800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010900000616     C                   PARM                    depcmd
011000000616     C                   PARM                    LENGH
011100000616     C*
011200020725     C                   if        not %open(titv19ps)
011300020725     C                   open      titv19ps
011400000616     C                   except    testdett
011500000613     C                   endif
011600000613     C*
011700020724     C                   EXSR      RWGLP                                        LETT/SCR. VAB
011800000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
011900000613     C*
012000010202     C* Effettuo la chiamata al CLLE preposto
012100050128     C                   call(e)   'TITVVTC'
012200010202     C                   parm                    parccm
012300010202     C                   parm                    parmbr
012400010202     C                   parm      '2'           paropz
012500010202     C*
012600020725     C                   if        %open(titv19ps)
012700000616     C                   except    findett
012800020725     C                   close     titv19ps
012900000613     C                   endif
013000000616     C*
013100000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
013200011113     C                   Z-ADD     150           LENGH            15 5
013300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013400000616     C                   PARM                    CMD(3)
013500000616     C                   PARM                    LENGH
013600000616     C*
013700000801     C
013800010201     C                   seton                                        LR
013900000613
014000000613     C*--------------------------------------------------------
014100000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
014200000613     C*--------------------------------------------------------
014300000621     C     STPR          BEGSR
014400000613     C*
014500020725     C                   if        not %open(titv19p)
014600020725     C                   open      titv19p
014700990915     C                   endif
014800990915     C*
014900990915     C                   except    riepilogo
015000990915     C*
015100020725     C                   if        %open(titv19p)
015200020725     C                   close     titv19p
015300990914     C                   endif
015400990910     C*
015500000613     C                   ENDSR
015600000613     C***
015700990908
015800000801
015900910830     C*--------------------------------------------------------
016000021113     C* RWGLP    LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
016100910830     C*--------------------------------------------------------
016200020724     C     RWGLP         BEGSR
016300990910     C*
016400990914     C                   if        not %open(tivin00r)
016500990908     C                   open      tivin00r
016600990914     C                   endif
016700021113     C                   if        not %open(fivabwwr)
016800021113     C                   open      fivabwwr
016900990914     C                   endif
017000021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
017100020305     C                   exsr      prevat
017200010201     C*
017300010202     C                   if        chkcall = '0'
017400010202     C*
017500021113     C                   if        not %open(fivatwwr)
017600021113     C                   open      fivatwwr
017700010201     C                   endif
017800990910     C*
017900010201     C                   clear                   §CTROKVB          5 0
018000020305     C                   clear                   §CTROKVT          5 0
018100000801     C                   clear                   §CTRMO            5 0
018200000801     C                   clear                   §CTRNO            5 0
018300990910     C*
018400020725     C*
018500020725     C/EXEC SQL
018600020725     C+ declare C1 cursor for select
018700110728     C+ vindta, vinflg, substr(vindta, 51, 7) as sped, rrn(tivin00r)
018800060223     C+ from tivin00r
018900060223     C+ where substr(vindta, 262, 23) <> ' '
019000060223     C+ order by sped
019100020725     C+ for read only
019200020725     C/END-EXEC
019300020725     C
019400020725     C/EXEC SQL
019500020725     C+ open C1
019600020725     C/END-EXEC
019700020725     C
019800020725     C/EXEC SQL
019900020725     C+ Fetch C1 into :PERLAGLPDS
020000020725     C/END-EXEC
020100020725     C*
020200020725     C                   dow       sqlcod = *zeros
020300990913     C*
020400020725     C                   if        vindta > *blanks
020500000613     C                   add       1             rrnum
020600000801     C*
020700020725     C                   if        vinflg = *blanks
020800020725     C                             or vinflg = '0'
020900020725     C                             or vinflg = '2'
021000000801     C*
021100020725     C                   clear                   x_vinmsg
021200020725     C                   eval      x_vinflg = '1'
021300010305     C*
021400010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
021500110728     C                   EVAL      PiStr=%trim(%subst(vindta:51:7))
021600020305     C                   MOVEL(p)  PiStr         curspe
021700010305     C*
021800010305     C                   if        depspe = *blanks                             => 1° giro
021900010305     C                   eval      depspe = curspe                              => memorizz. spediz
022000020725     C                   clear                   tivinds
022100020725     C                   exsr      inzvar
022200020725     C                   exsr      defcam
022300020305     C                   exsr      impvab
022400020305     C                   exsr      wrivat                                       => carico VAT
022500010305     C                   else
022600020725     C                   if        depspe <> curspe                             => rottura di spediz
022700010305     C                   eval      depspe = curspe                              => memorizz. spediz
022800020305     C                   exsr      wrivab
022900020725     C                   clear                   tivinds
023000020725     C                   exsr      inzvar
023100020725     C                   exsr      defcam
023200020305     C                   exsr      impvab
023300020305     C                   exsr      wrivat                                       => carico VAT
023400020305     C                   else                                                   => x stessa spediz
023500020305     C                   exsr      impvab
023600020305     C                   exsr      wrivat                                       => carico VAT
023700010305     C                   endif
023800010305     C                   endif
023900010305     C                   endif
024000000905     C*
024100000905     C                   else
024200020725     C                   eval      x_vinflg = '1'
024300000905     C                   endif
024400000905     C*
024500020725     C     VINRRN        chain     tivin000
024600020725     C                   if        %found(tivin00r)
024700020725     C                   eval      y_vinflg = x_vinflg
024800020725     C                   eval      y_vinmsg = x_vinmsg
024900020725     C                   update    tivin000
025000020725     C                   endif
025100020725     C*
025200020725     C/EXEC SQL
025300020725     C+ Fetch C1 into :PERLAGLPDS
025400020725     C/END-EXEC
025500020725     C*
025600020725     C                   enddo
025700020725     C*
025800020725     C/EXEC SQL
025900020725     C+ close C1
026000020725     C/END-EXEC
026100000905     C*
026200020305     C* Scarico i VAB rimasti "in sospeso"
026300020305     C                   exsr      wrivab
026400010202     C*
026500010202     C                   endif
026600990910
026700990910     C* Se non ci sono record con errori ...
026800000710     C                   if        §ctrno = 0
026900990910     C* ... restituisco esito OK.
027000990921     C                   eval      wrkesito = '0'
027100990910     C                   else
027200010201     C                   if        §ctrokvb > 0
027300990921     C                   eval      wrkesito = '1'
027400000710     C                   else
027500000710     C                   eval      wrkesito = '2'
027600990910     C                   endif
027700000710     C                   endif
027800990910     C*
027900990914     C                   if        %open(tivin00r)
028000990908     C                   close     tivin00r
028100990914     C                   endif
028200021113     C                   if        %open(fivabwwr)
028300021113     C                   close     fivabwwr
028400990914     C                   endif
028500021113     C                   if        %open(fivatwwr)
028600021113     C                   close     fivatwwr
028700010201     C                   endif
028800990910     C*
028900010201     C                   if        §ctrokvb > 0
029000000724     C                             and vlrpoi <> *zeros
029100010202     C                   exsr      invio
029200990920     C                   endif
029300990920     C*
029400910830     C                   ENDSR
029500000613     C***
029600010305
029700010305     C*----------------------------------------------------*
029800020305     C*  SCARICAMENTO BUFFER RECORDS VAB
029900010305     C*----------------------------------------------------*
030000020305     C     WRIVAB        BEGSR
030100010305     C*
030200021113     C                   write     fivab000                                     => scarico il VAB
030300010305     C*
030400010305     C                   ENDSR
030500990920
030600000801     C*----------------------------------------------------*
030700000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
030800000801     C*----------------------------------------------------*
030900010201     C     INZVAR        BEGSR
031000000801     C*
031100010201     C                   Z-ADD     *zeros        Num5_0
031200020725     C                   MOVEL     *blanks       WrkAAS            4
031300020725     C                   MOVEL     *blanks       WrkMGS            4
031400020725     C                   Z-ADD     *zeros        CntDDT            7 0
031500020725     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
031600020725     C                   MOVEL     '0'           FlgCAS            1
031700000801     C*
031800000801     C                   ENDSR
031900000801     C*----------------------------------------------------*
032000000801     C*  IMPOSTAZIONE CAMPI COSTANTI
032100000801     C*----------------------------------------------------*
032200020904     C     DEFCAM        BEGSR
032300000801     C*
032400021113     C                   CLEAR                   FIVAB000
032500021113     C                   CLEAR                   FIVAT000
032600020801     C                   Z-ADD     0011658       VABCCM
032700020801     C                   Z-ADD     0011658       VATCCM
032800020725     C                   Z-ADD     001           VABLNP
032900020725     C                   Z-ADD     001           VATLNP
033000010201     C                   Z-ADD     000           VABCTR
033100020305     C                   MOVEL     '7Q'          VABCTM
033200070222     C                   MOVEL     'C'           VABTSP
033300020305     C                   MOVEL     'E'           VATTRC
033400070222     C* ... e poi verifico se sono stati passati come parametri
033500070222     C                   IF        vlrppt > *blanks
033600070222     C                   IF        %subst(vlrppt:1:7) <> *blanks
033700070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
033800070222     C                   EXSR      CHKNUM
033900070222     C                   IF        PiInt=*on
034000070222     C                   Z-ADD     PiVal         VABCCM
034100070222     C                   Z-ADD     PiVal         VATCCM
034200070222     C                   ENDIF
034300070222     C                   ENDIF
034400070222     C                   IF        %subst(vlrppt:8:3) <> *blanks
034500070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
034600070222     C                   EXSR      CHKNUM
034700070222     C                   IF        PiInt=*on
034800070222     C                   Z-ADD     PiVal         VABLNP
034900070222     C                   Z-ADD     PiVal         VATLNP
035000070222     C                   ENDIF
035100070222     C                   ENDIF
035200070222     C                   IF        %subst(vlrppt:11:3) <> *blanks
035300070222     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
035400070222     C                   EXSR      CHKNUM
035500070222     C                   IF        PiInt=*on
035600070222     C                   Z-ADD     PiVal         VABCTR
035700070222     C                   ENDIF
035800070222     C                   ENDIF
035900070222     C                   ENDIF
036000000801     C*
036100000801     C                   ENDSR
036200000801     C*----------------------------------------------------*
036300021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
036400000801     C*----------------------------------------------------*
036500010201     C     IMPVAB        BEGSR
036600010305     C*
036700000801     C                   Z-ADD     *zeros        errore            1 0
036800021113     C*
036900021113     C                   MOVE(P)   vlrpoi        VABFGS
037000021113     C                   MOVE(P)   vlrpoi        VATFGS
037100020725     C*
037200020725     C* Campi anno e mese/giorno
037300020725     C                   EVAL      WrkAAS=%subst(vindta:24:4)
037400020725     C                   MOVEL(P)  WrkAAS        VABAAS
037500020725     C                   MOVEL(P)  WrkAAS        VATAAS
037600020725     C                   EVAL      WrkMGS=%subst(vindta:29:2) +
037700020725     C                                    %subst(vindta:32:2)
037800020725     C                   MOVEL(P)  WrkMGS        VABMGS
037900020725     C*
038000020725     C* Campi relativi al destinatario
038100020725     C                   EVAL      VABRSD=%trim(%subst(vindta:81:35))
038200020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
038300020117     C     '@':'A'       XLATE     VABRSD        VABRSD
038400020117     C* ==
038500020725     C                   EVAL      VABIND=%trim(%subst(vindta:116:35))
038600020725     C                   EVAL      VABLOD=%trim(%subst(vindta:151:35))
038700020725     C                   EVAL      VABNZD=%trim(%subst(vindta:196:4))
038800020725     C                   IF        %trim(VABNZD) = 'I'
038900020725     C                   EVAL      VABNZD = *blanks
039000020725     C                   ENDIF
039100020725     C                   EVAL      VABPRD=%trim(%subst(vindta:203:3))
039200020725     C                   EVAL      VABRMA=%trim(%subst(vindta:309:10))
039300050103     C                   EVAL      VABNT2=%trim(%subst(vindta:329:25))
039400020725     C*
039500020725     C* Altri campi alfa "costruiti"
039600020725     C                   EVAL      VABRMO=%subst(vindta:11:3) +
039700020725     C                                    %subst(vindta:14:7) +
039800020904     C                                    %subst(vindta:21:3) +
039900020904     C                                    %subst(vindta:312:7)
040000020725     C                   IF        %subst(vindta:306:1) = 'E'
040100020725     C                   EVAL      VABTSP='E'
040200020725     C                   ENDIF
040300020725     C                   EVAL      VABVCA=%trim(%subst(vindta:302:4))
040400020725     C*
040500020725     C* Effettuo il conteggio dei DDT x spedizione per indicare eventualmente nelle note
040600020725     C                   IF        %subst(vindta:307:1) = 'S'
040700020725     C                   ADD       1             CntDDT
040800020725     C                   ENDIF
040900020725     C                   IF        CntDDT > 1
041000020725     C                   EVAL      VABNOT = 'NUMERO DI DDT NELLA SPEDIZIONE: ' +
041100020725     C                                      %editc(CntDDT:'1')
041200020725     C                   ENDIF
041300050103     C*
041400050103     C* Effettuo considerazioni relative alle consegne particolari
041500050103     C                   IF        %subst(vindta:308:1) = 'G'
041600050103     C                   EVAL      VABTC1 = 'S'
041700050103     C                   ENDIF
041800050103     C                   IF        %subst(vindta:308:1) = 'P'
041900050103     C                   EVAL      VABTC1 = 'A'
042000050103     C                   ENDIF
042100050103     C                   IF        %subst(vindta:308:1) = 'S'
042200050103     C                   EVAL      VABTC1 = 'P'
042300050103     C                   ENDIF
042400020725     C*
042500020725     C* Campi numerici
042600020725     C                   EVAL      PiStr=%trim(%subst(vindta:186:10))
042700000801     C                   EXSR      CHKNUM
042800000801     C                   IF        PiInt=*on
042900000801     C                   Z-ADD     PiVal         Num5_0
043000010208     C                   MOVEL(p)  Num5_0        VABCAD
043100000801     C                   ELSE
043200000801     C                   ADD       1             errore
043300020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
043400000801     C                             + ' ' + 'VABCAD'
043500000801     C                   ENDIF
043600020725     C*
043700020725     C                   EVAL      PiStr=%trim(%subst(vindta:235:7))
043800010201     C                   EXSR      CHKNUM
043900010201     C                   IF        PiInt=*on
044000020725     C                   ADD       PiVal         VABNCL
044100010201     C                   ELSE
044200010201     C                   ADD       1             errore
044300020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
044400010201     C                             + ' ' + 'VABNCL'
044500010201     C                   ENDIF
044600020305     C*
044700020725     C                   EVAL      PiStr=%trim(%subst(vindta:251:11))
044800010201     C                   EXSR      CHKNUM
044900010201     C                   IF        PiNum=*on
045000020725     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
045100020725     C* riferisce ad un campo 11,3
045200020725     C                   EVAL(H)   VABPKB = VABPKB + (PiVal/1000)
045300010201     C                   ELSE
045400010201     C                   ADD       1             errore
045500020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
045600010201     C                             + ' ' + 'VABPKB'
045700010201     C                   ENDIF
045800020305     C*
045900020725     C                   EVAL      PiStr=%trim(%subst(vindta:242:9))
046000010201     C                   EXSR      CHKNUM
046100010201     C                   IF        PiNum=*on
046200030305     C*
046300030305     C* Verifico (visti i precedenti) che il vol. ottenuto dal fle del cliente nn superi i 99,999 M3
046400030305     C                   IF        PiVal <= 99999
046500020725     C* Divido x 1000 perchè il campo di input non gestisce separatori decimali ma il valore si
046600020725     C* riferisce ad un campo 9,3
046700020725     C                   EVAL(H)   VABVLB = VABVLB + (PiVal/1000)
046800010201     C                   ELSE
046900010201     C                   ADD       1             errore
047000020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047100010201     C                             + ' ' + 'VABVLB'
047200010202     C                   ENDIF
047300030305     C                   ELSE
047400030305     C                   ADD       1             errore
047500030305     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
047600030305     C                             + ' ' + 'VABVLB'
047700030305     C                   ENDIF
047800020305     C*
047900110728     C                   EVAL      PiStr=%trim(%subst(vindta:51:7))
048000010201     C                   EXSR      CHKNUM
048100010201     C                   IF        PiInt=*on
048200011115     C                   Z-ADD     PiVal         VABNSP
048300020305     C                   Z-ADD     PiVal         VATNSP
048400010201     C                   ELSE
048500010201     C                   ADD       1             errore
048600020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
048700050112     C                             + ' ' + 'VABNSP VATNSP'
048800010201     C                   ENDIF
048900020904     C*
049000050112     C                   EVAL      PiStr=%trim(%subst(vindta:51:7))
049100050112     C                   EXSR      CHKNUM
049200050112     C                   IF        PiInt=*on
049300050112     C                   Z-ADD     PiVal         VABRMN
049400050112     C                   ELSE
049500050112     C                   ADD       1             errore
049600050112     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
049700050112     C                             + ' ' + 'VABRMN'
049800050112     C                   ENDIF
049900020725     C*
050000020725     C                   EVAL      PiStr=%trim(%subst(vindta:285:17))
050100020725     C* Detrmino se indipendentemente da giusto o sbagliato l'intenzione del cliente era d
050200020725     C* passare il contrassegno
050300020725     C                   IF        PiStr > *zeros
050400020725     C                   MOVEL     '1'           FlgCAS
050500020725     C                   ENDIF
050600020725     C*
050700020725     C                   EXSR      CHKNUM
050800020725     C                   IF        PiNum=*on
050900020725     C* Divido x 100000 perchè il campo di input non gestisce separatori decimali ma il valore si
051000020725     C* riferisce ad un campo 17,5
051100020725     C                   EVAL(H)   VABCAS = VABCAS + (PiVal/100000)
051200020725     C                   ELSE
051300020725     C                   SETON                                        32
051400020725     C                   Z-ADD     *zeros        VABCAS
051500020725     C                   EVAL      x_vinmsg = %trimr(x_vinmsg)
051600020725     C                             + ' ' + 'VABCAS'
051700020725     C                   ENDIF
051800010205     C*
051900020725     C* Considerazioni sul contenuto di campi precedentemente valorizzati in relazione
052000020725     C* al tipo porto ed al contrassegno => x stabilire il tipo bolla
052100020725     C                   IF        %trim(%subst(vindta:233:2)) = '01' AND
052200020725     C                             FlgCAS = '0'
052300020904     C                   EVAL      VABCBO = '2'
052400020725     C                   ENDIF
052500020725     C                   IF        %trim(%subst(vindta:233:2)) = '01' AND
052600020725     C                             FlgCAS = '1'
052700020904     C                   EVAL      VABCBO = '6'
052800020725     C                   ENDIF
052900020725     C                   IF        %trim(%subst(vindta:233:2)) = '02' AND
053000020725     C                             FlgCAS = '0'
053100020904     C                   EVAL      VABCBO = '1'
053200020725     C                   ENDIF
053300020904     C                   IF        %trim(%subst(vindta:233:2)) = '02' AND
053400020725     C                             FlgCAS = '1'
053500020904     C                   EVAL      VABCBO = '4'
053600020725     C                   ENDIF
053700020725     C                   IF        %trim(%subst(vindta:233:2)) = '74'
053800020725     C                   EVAL      VABCBO = 'A'
053900020725     C                   ENDIF
054000020305     C*
054100011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
054200011113     C                   EXSR      CHKIMPDIV
054300010202     C*
054400000801     C* Ebbene...
054500000801     C                   ADD       1             §CTRMO
054600010201     C                   IF        errore <> *zeros
054700000801     C                   ADD       1             §CTRNO
054800000801     C                   EVAL      recko = vindta
054900000801     C                   EXCEPT    rigadett
055000020725     C                   EVAL      x_vinflg = '2'
055100000801     C                   ELSE
055200010201     C                   ADD       1             §CTROKVB
055300000801     C                   ENDIF
055400000801     C*
055500000801     C                   ENDSR
055600010201     C*----------------------------------------------------*
055700021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
055800010201     C*----------------------------------------------------*
055900020305     C     WRIVAT        BEGSR
056000010201     C*
056100021113     C* Valorizzo l buffer di scrittura del FIVAT
056200020725     C                   if        %subst(vindta:262:23) <> *blanks
056300020725     C                   eval      VATNOT = %trim(%subst(vindta:262:23))
056400040916     C* X alcuni barcode si rende necessario calcolare il check-digits (solo quelli numerici)
056500040916     C***                if        %subst(%trim(VATNOT):1:3) <> 'DAL'
056600040916     C                   eval      PiStr=%subst(VATNOT:1:3)
056700040916     C                   exsr      CHKNUM
056800040916     C                   if        PiInt=*on
056900020904     C                   exsr      CALCHKDGT
057000020904     C                   eval      VATNOT = %trim(VATNOT) + WrkChkDgtAlfa
057100020912     C                   endif
057200021113     C                   write     FIVAT000
057300020725     C                   else
057400020725     C                   eval      VABCTM = *blanks
057500020725     C                   endif
057600010201     C*
057700010201     C                   ENDSR
057800020904
057900020904
058000020904     C*--------------------------------------------------------
058100020904     C* CALCHKDGT - ROUTINE DI CALCOLO CHECK-DIGITS (MODULO 10)
058200020904     C*--------------------------------------------------------
058300020904     C     CALCHKDGT     BEGSR
058400020904     C*
058500020904     C                   CLEAR                   SkChkDgt
058600020904     C                   MOVEA     VATNOT        SkChkDgt
058700020904     C* Inizializzo variabili di wrk
058800020904     C                   Z-ADD     *zeros        WrkChkDgt         1 0
058900020904     C                   Z-ADD     *zeros        WrkChkDgtNum      1 0
059000020904     C                   MOVEL     *blanks       WrkChkDgtAlfa     1
059100020904     C                   Z-ADD     *zeros        SommaPari         5 0
059200020904     C                   Z-ADD     *zeros        SommaDispari      5 0
059300020904     C                   Z-ADD     *zeros        SommaTot          5 0
059400020904     C                   Z-ADD     *zeros        Risultato         3 1
059500020904     C*
059600020904     C* Effettuo il calcolo
059700020904     C     1             DO        35            I                 2 0
059800020904     C                   IF        SkChkDgt(I) <> *blanks
059900020904     C* Stabilisco se trattasi di cifra in posizione pari o dispari
060000020904     C                   MOVEL(P)  SkChkDgt(I)   WrkChkDgt
060100020904     C                   EVAL      Risultato = I/2
060200020904     C                   Z-ADD     Risultato     RestoDec          1 1
060300020904     C                   IF        RestoDec = *zeros                            * è pari
060400020904     C                   ADD       WrkChkDgt     SommaPari
060500020904     C                   ELSE                                                   * è dispari
060600020904     C                   ADD       WrkChkDgt     SommaDispari
060700020904     C                   ENDIF
060800020904     C                   ENDIF
060900020904     C                   ENDDO
061000020904     C*
061100020904     C                   EVAL      SommaTot = (SommaDispari * 3) + SommaPari
061200020904     C                   Z-ADD     *zeros        Multiplo10        5 0
061300020904     C                   DOW       Multiplo10 < SommaTot
061400020904     C                   EVAL      Multiplo10 = Multiplo10 + 10
061500020904     C                   ENDDO
061600020904     C*
061700020904     C                   EVAL      WrkChkDgtNum = Multiplo10 - SommaTot
061800020904     C                   MOVEL     WrkChkDgtNum  WrkChkDgtAlfa
061900020904     C*
062000020904     C                   ENDSR
062100020904     C***
062200020904
062300020904
062400010202     C*----------------------------------------------------*
062500021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
062600010202     C*----------------------------------------------------*
062700020305     C     PREVAT        BEGSR
062800010202     C*
062900021113     C* Compongo il nome del membro da dare al FIVATWWR
063000010202     C                   eval      parmbr = vlrhdl
063100010202     C                   movel     'M'           parmbr
063200020801     C                   eval      parccm = '00011658'
063300010202     C                   eval      paropz = '1'
063400010202     C* Effettuo la chiamata al CLLE preposto
063500050128     C                   call(e)   'TITVVTC'
063600010202     C                   parm                    parccm
063700010202     C                   parm                    parmbr
063800010202     C                   parm                    paropz
063900010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
064000010202     C                   if        %error
064100010202     C                   movel     '1'           chkcall
064200010202     C                   else
064300010202     C                   movel     '0'           chkcall
064400010202     C                   endif
064500010202     C*
064600010202     C                   ENDSR
064700000801     C*----------------------------------------------------*
064800000801     C*  CONTROLLO NUMERICITA' CAMPI
064900000801     C*----------------------------------------------------*
065000000801     C     CHKNUM        BEGSR
065100000801     C*
065200000801     C                   call(e)   'ISNUMERIC'
065300000801     C                   PARM                    PiStr            30
065400000801     C                   PARM      ','           PiDecChr          1
065500000801     C                   PARM      *ZEROS        PiVal            30 9
065600000801     C                   PARM      '0'           PiInt             1
065700000801     C                   PARM      '0'           PiNum             1
065800000801     C                   IF        %error
065900000801     C                   EVAL      PiInt=*off
066000000801     C                   ENDIF
066100000801     C*
066200000801     C                   ENDSR
066300000801     C***
066400000801
066500011113
066600011113     C*----------------------------------------------------*
066700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
066800011113     C*----------------------------------------------------*
066900011113     C     CHKIMPDIV     BEGSR
067000011113     C*
067100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
067200011113     C                   Z-ADD     *zeros        wrkDec            9 9
067300011113     C*
067400011113     C* Come prima cosa effettuo considerazioni sulla divisa
067500011113     C                   IF        vabIAS > *zeros
067600011113     C                   IF        vabVAS <> 'EUR'
067700011113     C                   EVAL      vabVAS =  'ITL'
067800011113     C                   ENDIF
067900011113     C                   ENDIF
068000011113     C*
068100011113     C                   IF        vabCAS > *zeros
068200011113     C                   IF        vabVCA <> 'EUR'
068300011113     C                   EVAL      vabVCA =  'ITL'
068400011113     C                   ENDIF
068500011113     C                   ENDIF
068600011113     C*
068700011113     C                   IF        vabVMD > *zeros
068800020305     C                   IF        vabVAD <> 'EUR'
068900011113     C                   EVAL      vabVAD =  'ITL'
069000011113     C                   ENDIF
069100011113     C                   ENDIF
069200011113     C*
069300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
069400011113     C                   Z-ADD     vabIAS        wrkDec
069500011113     C                   IF        wrkDec > *zeros
069600011113     C                   IF        vabVAS = 'ITL'
069700011113     C                   EVAL      vabIAS = *zeros
069800011113     C                   ENDIF
069900011113     C                   ENDIF
070000011113     C*
070100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
070200011113     C                   Z-ADD     vabCAS        wrkDec
070300011113     C                   IF        wrkDec > *zeros
070400011113     C                   IF        vabVCA = 'ITL'
070500011113     C                   EVAL      vabCAS = *zeros
070600011113     C                   ENDIF
070700011113     C                   ENDIF
070800011113     C*
070900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
071000011113     C                   Z-ADD     vabVMD        wrkDec
071100011113     C                   IF        wrkDec > *zeros
071200011113     C                   IF        vabVAD = 'ITL'
071300011113     C                   EVAL      vabVMD = *zeros
071400011113     C                   ENDIF
071500011113     C                   ENDIF
071600011113     C*
071700011113     C                   ENDSR
071800011113     C***
071900011113
072000011113
072100000801
072200000801
072300990920      /TITLE Invio dei dati al punto operativo.
072400010202     C     invio         BEGSR
072500990920     C*
072600021113     C* 1° invio FIVAT
072700010201     C                   reset                   dscmz
072800010201     C                   move      vlrpoi        cmzdst
072900021113     C                   eval      cmzfld = 'FIVATWWR'
073000010201     C                   eval      cmzmbd = vlrhdl
073100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
073200021009     C***                if        prmfir = *blanks
073300021113     C                   eval      cmzfla = 'FIVAT00F'
073400021113     C                   eval      cmzmba = 'FIVAT00F'
073500021009     C***                else
073600021009     C***                eval      cmzfla = prmfir
073700021009     C***                eval      cmzmba = prmfir
073800021009     C***                endif
073900010201     C                   eval      cmznrr = *zeros
074000020305     C                   move      §ctrokvt      cmznrr
074100021018     C                   eval      cmzlba = vlrfl1
074200010201     C                   call(e)   'TIS711C'
074300010201     C                   parm                    dscmz
074400010201     C                   parm      *blanks       esito
074500010205     C                   if        %error
074600010205     C                             or cmzerr = '1'
074700010205     C                             or esito  = '1'
074800010205     C                   eval      wrkesito = '3'
074900010205     C                   else
075000010201     C*
075100021113     C* 2° invio FIVAB
075200010201     C                   reset                   dscmz
075300010201     C                   move      vlrpoi        cmzdst
075400010201     C                   eval      cmzfld = vlrfou
075500010201     C                   eval      cmzmbd = vlrhdl
075600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
075700021009     C***                if        prmfir = *blanks
075800021113     C                   eval      cmzfla = 'FIVAB00F'
075900021113     C                   eval      cmzmba = 'FIVAB00F'
076000021009     C***                else
076100021009     C***                eval      cmzfla = prmfir
076200021009     C***                eval      cmzmba = prmfir
076300021009     C***                endif
076400010201     C                   eval      cmznrr = *zeros
076500010201     C                   move      §ctrokvb      cmznrr
076600021018     C                   eval      cmzlba = vlrfl1
076700010201     C                   call(e)   'TIS711C'
076800010201     C                   parm                    dscmz
076900010201     C                   parm      *blanks       esito
077000010201     C                   if        %error
077100010201     C                             or cmzerr = '1'
077200010201     C                             or esito  = '1'
077300010201     C                   eval      wrkesito = '3'
077400010201     C                   endif
077500010205     C                   endif
077600990920     C*
077700000613     C                   ENDSR
077800000613     C***
077900070411
078000070411     C     *pssr         BEGSR
078100070411     C*
078200070411     C                   if        %open(tivin00r)
078300070411     C                   close     tivin00r
078400070411     C                   endif
078500070411     C                   if        %open(fivabwwr)
078600070411     C                   close     fivabwwr
078700070411     C                   endif
078800070411     C                   if        %open(fivatwwr)
078900070411     C                   close     fivatwwr
079000070411     C                   endif
079100070411     C*
079200070411     C* Effettuo la chiamata al CLLE preposto
079300070411     C                   call(e)   'TITVVTC'
079400070411     C                   parm                    parccm
079500070411     C                   parm                    parmbr
079600070411     C                   parm      '2'           paropz
079700070411     C*
079800070411     C                   eval      wrkesito = '2'
079900070411     C*
080000070411     C                   seton                                        LR
080100070411     C*
080200070411     C                   ENDSR     '*CANCL'
080300070411     C***
080400070411
080500990910
080600000613     C     *inzsr        BEGSR
080700990910     C*
080800990910     C     *entry        plist
080900990920     C                   parm                    tivlrds
081000990921     C                   parm      wrkesito      esito
081100000724     C                   parm                    prmlit
081200000710     C                   parm                    prmfir
081300000613     C*
081400000830     C* CALCOLA LA DATA CORRENTE
081500000830     C                   time                    wn14             14 0
081600000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
081700000830     C                   z-add     wn8           g08dat
081800000830     C                   z-add     *zeros        g08inv
081900000830     C                   movel     '0'           g08err
082000000830     C                   call      'XSRDA8'
082100000830     C                   parm                    wlbda8
082200000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
082300000830     C*
082400000613     C                   ENDSR
082500000613     C***
082600990908
082700020725     Otitv19p   E            riepilogo         2
082800990915     O                                              'Upload via Internet'
082900990915     O                                           +1 'Traduzione TIVIN00R -'
083000021113     O                                           +1 'FIVABWWR/FIVATWWR'
083100010201     O                                           +1 'Report testate bolle'
083200990915     O          E            riepilogo   2
083300990915     O                       wrkdata
083400990915     O                       wrkora              +1
083500990915     O                       procname            +1
083600990915     O          E            riepilogo   2
083700990915     O                                              'Cliente..................:'
083800990915     O                       VABCCM        z     +1
083900990915     O          E            riepilogo   2
084000990920     O                                              'Riferimento Strategi.....:'
084100990920     O                       vlrhdl              +1
084200990915     O          E            riepilogo   2
084300990915     O                                              'Giusti...................:'
084400010201     O                       §CTROKVB      2   +  1
084500990915     O          E            riepilogo   2
084600990915     O                                              'Sbagliati e corretti.....:'
084700971022     O                       §CTRMO        2   +  1
084800990915     O          E            riepilogo   2
084900990915     O                                              'Sbagliati e scartati.....:'
085000971022     O                       §CTRNO        2   +  1
085100000613
085200020725     Otitv19ps  E            testdett          2
085300000613     O                                              'Upload via Internet'
085400000613     O                                           +1 'Traduzione TIVIN00R -'
085500021113     O                                           +1 'FIVABWWR/FIVATWWR'
085600010201     O                                           +1 'Report testate bolle'
085700000616     O          E            testdett    3
085800000613     O                                           +2 'N° rec'
085900000613     O                                           +3 'Anteprima contenuto'
086000000616     O          E            rigadett    2
086100000613     O                       rrnum               +2
086200000621     O                       recko               +3
086300000616     O          E            findett     2
086400000613     O                       wrkdata
086500000613     O                       wrkora              +1
086600000613     O                       procname            +1
086700000616     O          E            findett     2
086800000613     O                                              'Cliente..................:'
086900000613     O                       VABCCM        z     +1
087000000616     O          E            findett     2
087100000613     O                                              'Riferimento Strategi.....:'
087200000613     O                       vlrhdl              +1
087300000616     O          E            findett     2
087400000613     O                                              'Giusti...................:'
087500010201     O                       §CTROKVB      2   +  1
087600000616     O          E            findett     2
087700000613     O                                              'Sbagliati e corretti.....:'
087800000613     O                       §CTRMO        2   +  1
087900000616     O          E            findett     2
088000000613     O                                              'Sbagliati e scartati.....:'
088100000613     O                       §CTRNO        2   +  1
088200000512** CMD - COMANDI CL
088300020725OVRPRTF FILE(TITV19P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
088400020725OVRPRTF FILE(TITV19PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
088500020725DLTOVR FILE(TITV19P TITV19PS) LVL(*)
088600000512
088700000512
