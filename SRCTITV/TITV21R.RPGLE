000100021113      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021113     FFIVABwwr  O    E             DISK    usropn
000700021113     FFIVATwwr  O    E             DISK    usropn
000800020801     Ftitv21p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100020801     Ftitv21ps  O    f  198        PRINTER usropn
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
003200020801     D wrkNOTnum       s              4  0
003300020801     D wrkNOTalfa      s              4
003400020801     D wrkNCDnum       s              4  0
003500020801     D wrkNCDalfa      s              4
003600020801     D wrkNCAnum       s              4  0
003700020801     D wrkNCAalfa      s              4
003800000613     D rrnum           s              6  0 INZ(*zeros)
003900000621     D recko           s            150    INZ(*blanks)
004000011113     D depcmd          s            150    INZ(*blanks)
004100020305     D depspe          s             14    INZ(*blanks)
004200010213     D depdat          s              8  0 INZ(*zeros)
004300020305     D curspe          s             14    INZ(*blanks)
004400010202     D parccm          s              8    INZ(*blanks)
004500010202     D parmbr          s             10    INZ(*blanks)
004600010202     D paropz          s              1    INZ(*blanks)
004700010202     D chkcall         s              1    INZ(*blanks)
004800010213
004900000801     D  Num5_0         s              5  0
005000000830
005100000830     D*------------------
005200000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
005300000830     D*------------------
005400000830     D WLBDA8          DS                  INZ
005500000830     D  G08DAT                 1      8  0
005600000830     D  G08INV                 9     16  0
005700000830     D  G08ERR                17     17
005800000830     D  G08TGI                18     22  0
005900000830     D*
006000990908
006100010201
006200010201
006300990915     C                   time                    wrkdata
006400990915     C                   time                    wrkora
006500000913     C                   reset                   rrnum
006600990921     C                   reset                   esito
006700990921     C                   reset                   wrkesito
006800000724     C*
006900000724     C* SE OCCORRE SPEDIRE IN FILIALE
007000000724     C                   if        vlrpoi <> *zeros
007100000724     C*
007200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007400000724     C     vlrpoi        chain     azorg01l
007500000724     C                   if        %found
007600000616     C                   movel(p)  CMD(1)        depcmd
007700020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
007800000616     C*
007900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008000011113     C                   Z-ADD     150           LENGH            15 5
008100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008200000616     C                   PARM                    depcmd
008300000616     C                   PARM                    LENGH
008400000724     C*
008500000724     C                   endif
008600000724     C                   endif
008700000616     C*
008800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
008900000616     C                   movel(p)  CMD(2)        depcmd
009000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009100000616     C*
009200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009300011113     C                   Z-ADD     150           LENGH            15 5
009400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009500000616     C                   PARM                    depcmd
009600000616     C                   PARM                    LENGH
009700000616     C*
009800020801     C                   if        not %open(titv21ps)
009900020801     C                   open      titv21ps
010000000616     C                   except    testdett
010100000613     C                   endif
010200000613     C*
010300020801     C                   EXSR      RWMARES                                      LETT/SCR. VAB
010400000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010500000613     C*
010600010202     C* Effettuo la chiamata al CLLE preposto
010700050128     C                   call(e)   'TITVVTC'
010800010202     C                   parm                    parccm
010900010202     C                   parm                    parmbr
011000010202     C                   parm      '2'           paropz
011100010202     C*
011200020801     C                   if        %open(titv21ps)
011300000616     C                   except    findett
011400020801     C                   close     titv21ps
011500000613     C                   endif
011600000616     C*
011700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
011800011113     C                   Z-ADD     150           LENGH            15 5
011900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012000000616     C                   PARM                    CMD(3)
012100000616     C                   PARM                    LENGH
012200000616     C*
012300000801     C
012400010201     C                   seton                                        LR
012500000613
012600000613
012700000613     C*--------------------------------------------------------
012800000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
012900000613     C*--------------------------------------------------------
013000000621     C     STPR          BEGSR
013100000613     C*
013200020801     C                   if        not %open(titv21p)
013300020801     C                   open      titv21p
013400990915     C                   endif
013500990915     C*
013600990915     C                   except    riepilogo
013700990915     C*
013800020801     C                   if        %open(titv21p)
013900020801     C                   close     titv21p
014000990914     C                   endif
014100990910     C*
014200000613     C                   ENDSR
014300000613     C***
014400990908
014500000801
014600910830     C*--------------------------------------------------------
014700021113     C* RWMARES  LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR   *
014800910830     C*--------------------------------------------------------
014900020801     C     RWMARES       BEGSR
015000990910     C*
015100990914     C                   if        not %open(tivin00r)
015200990908     C                   open      tivin00r
015300990914     C                   endif
015400021113     C                   if        not %open(fivabwwr)
015500021113     C                   open      fivabwwr
015600990914     C                   endif
015700021113     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015800020305     C                   exsr      prevat
015900010201     C*
016000010202     C                   if        chkcall = '0'
016100010202     C*
016200021113     C                   if        not %open(fivatwwr)
016300021113     C                   open      fivatwwr
016400010201     C                   endif
016500990910     C*
016600010201     C                   clear                   §CTROKVB          5 0
016700020305     C                   clear                   §CTROKVT          5 0
016800000801     C                   clear                   §CTRMO            5 0
016900000801     C                   clear                   §CTRNO            5 0
017000990910     C*
017100921023     C                   DO        *HIVAL
017200990913     C*
017300990915     C                   READ      tivin00r                               70
017400000905     C                   if        vindta > *blanks
017500000613     C                   add       1             rrnum
017600000801     C*
017700000801     C                   if        *in70 = *off
017800000801     C                             and
017900000801     C                             (vinflg = *blanks
018000000801     C                              or vinflg = '0'
018100000801     C                              or vinflg = '2')
018200000801     C*
018300000801     C                   clear                   vinmsg
018400000801     C                   eval      vinflg = '1'
018500010305     C*
018600010305     C* Determino il numero di Spedizione e a rottura eseguo operazioni
018700020801     C                   EVAL      PiStr=%trim(%subst(vindta:4:7))
018800020305     C                   MOVEL(p)  PiStr         curspe
018900010305     C*
019000010305     C                   if        depspe = *blanks                             => 1° giro
019100010305     C                   eval      depspe = curspe                              => memorizz. spediz
019200020305     C                   exsr      impvab
019300020305     C                   exsr      wrivat                                       => carico VAT
019400010305     C                   else
019500010305     C                   if        curspe <> depspe                             => rottura di spediz
019600010305     C                   eval      depspe = curspe                              => memorizz. spediz
019700020305     C                   exsr      wrivab
019800020305     C                   exsr      impvab
019900020305     C                   exsr      wrivat                                       => carico VAT
020000020305     C                   else                                                   => x stessa spediz
020100020305     C                   exsr      impvab
020200020305     C                   exsr      wrivat                                       => carico VAT
020300010305     C                   endif
020400010305     C                   endif
020500010305     C                   endif
020600000905     C*
020700000905     C                   else
020800000905     C                   eval      vinflg = '1'
020900000905     C                   endif
021000000905     C*
021100000905     C  N70              update    tivin000
021200000905     C*
021300991022     C  N70              ENDdo
021400010202     C*
021500020305     C* Scarico i VAB rimasti "in sospeso"
021600020305     C                   exsr      wrivab
021700010202     C*
021800010202     C                   endif
021900990910
022000990910     C* Se non ci sono record con errori ...
022100000710     C                   if        §ctrno = 0
022200990910     C* ... restituisco esito OK.
022300990921     C                   eval      wrkesito = '0'
022400990910     C                   else
022500010201     C                   if        §ctrokvb > 0
022600990921     C                   eval      wrkesito = '1'
022700000710     C                   else
022800000710     C                   eval      wrkesito = '2'
022900990910     C                   endif
023000000710     C                   endif
023100990910     C*
023200990914     C                   if        %open(tivin00r)
023300990908     C                   close     tivin00r
023400990914     C                   endif
023500021113     C                   if        %open(fivabwwr)
023600021113     C                   close     fivabwwr
023700990914     C                   endif
023800021113     C                   if        %open(fivatwwr)
023900021113     C                   close     fivatwwr
024000010201     C                   endif
024100990910     C*
024200010201     C                   if        §ctrokvb > 0
024300000724     C                             and vlrpoi <> *zeros
024400010202     C                   exsr      invio
024500990920     C                   endif
024600990920     C*
024700910830     C                   ENDSR
024800000613     C***
024900010305
025000010305     C*----------------------------------------------------*
025100020305     C*  SCARICAMENTO BUFFER RECORDS VAB
025200010305     C*----------------------------------------------------*
025300020305     C     WRIVAB        BEGSR
025400010305     C*
025500021113     C                   write     fivab000                                     => scarico il VAB
025600010305     C*
025700010305     C                   ENDSR
025800990920
025900000801     C*----------------------------------------------------*
026000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
026100000801     C*----------------------------------------------------*
026200010201     C     INZVAR        BEGSR
026300000801     C*
026400010201     C                   Z-ADD     *zeros        Num5_0
026500000801     C*
026600000801     C                   ENDSR
026700000801     C*----------------------------------------------------*
026800000801     C*  IMPOSTAZIONE CAMPI COSTANTI
026900000801     C*----------------------------------------------------*
027000000801     C     DEFCAM        BEGSR
027100000801     C*
027200021113     C                   CLEAR                   FIVAB000
027300020619     C* Imposto i valori di default...
027400020801     C                   MOVEL     '1'           VABCBO
027500020801     C                   Z-ADD     0021122       VABCCM
027600020801     C                   Z-ADD     0021122       VATCCM
027700020801     C                   Z-ADD     002           VABLNP
027800020801     C                   Z-ADD     002           VATLNP
027900020801     C***                Z-ADD     300           VABCTR
028000020305     C                   MOVEL     '7Q'          VABCTM
028100020305     C                   MOVEL     'E'           VATTRC
028200020619     C* ... e poi verifico se sono stati passati come parametri
028300020619     C                   IF        vlrppt > *blanks
028400020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
028500020619     C                   EXSR      CHKNUM
028600020619     C                   IF        PiInt=*on
028700020619     C                   Z-ADD     PiVal         VABCCM
028800020619     C                   Z-ADD     PiVal         VATCCM
028900020619     C                   ENDIF
029000020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
029100020619     C                   EXSR      CHKNUM
029200020619     C                   IF        PiInt=*on
029300020619     C                   Z-ADD     PiVal         VABLNP
029400020619     C                   Z-ADD     PiVal         VATLNP
029500020619     C                   ENDIF
029600020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
029700020619     C                   EXSR      CHKNUM
029800020619     C                   IF        PiInt=*on
029900020619     C                   Z-ADD     PiVal         VABCTR
030000020619     C                   ENDIF
030100020619     C                   ENDIF
030200000801     C*
030300000801     C                   ENDSR
030400000801     C*----------------------------------------------------*
030500021113     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
030600000801     C*----------------------------------------------------*
030700010201     C     IMPVAB        BEGSR
030800000801     C*
030900020305     C                   EXSR      INZVAR
031000020305     C                   EXSR      DEFCAM
031100010305     C*
031200000801     C                   Z-ADD     *zeros        errore            1 0
031300000830     C                   MOVEL     datcor        VABAAS
031400020305     C                   MOVEL     datcor        VATAAS
031500000830     C                   MOVE      datcor        VABMGS
031600021113     C                   MOVE(P)   vlrpoi        VABFGS
031700021113     C                   MOVE(P)   vlrpoi        VATFGS
031800020801     C                   EVAL      VABRSD=%trim(%subst(vindta:25:35))
031900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
032000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
032100020117     C* ==
032200020801     C                   EVAL      VABIND=%trim(%subst(vindta:60:35))
032300020801     C                   EVAL      VABLOD=%trim(%subst(vindta:98:35))
032400020801     C                   EVAL      VABPRD=%trim(%subst(vindta:95:3))
032500020801     C                   EVAL      VABRMA=%trim(%subst(vindta:1:10))
032600020801     C*
032700020801     C                   EVAL      PiStr=%trim(%subst(vindta:133:10))
032800020801     C                   EXSR      CHKNUM
032900020801     C                   IF        PiInt=*on
033000020801     C                   Z-ADD     PiVal         Num5_0
033100020801     C                   MOVE      Num5_0        VABCAD
033200020801     C                   ELSE
033300020801     C                   ADD       1             errore
033400020801     C                   EVAL      vinmsg = %trimr(vinmsg)
033500020801     C                             + ' ' + 'VABCAD'
033600020801     C                   ENDIF
033700020305     C*
033800020801     C                   EVAL      PiStr=%trim(%subst(vindta:202:4))
033900010201     C                   EXSR      CHKNUM
034000010201     C                   IF        PiInt=*on
034100010201     C                   Z-ADD     PiVal         Num5_0
034200010208     C                   MOVE      Num5_0        VABNCL
034300010201     C                   ELSE
034400010201     C                   ADD       1             errore
034500010201     C                   EVAL      vinmsg = %trimr(vinmsg)
034600010201     C                             + ' ' + 'VABNCL'
034700010201     C                   ENDIF
034800020305     C*
034900020801     C                   EVAL      PiStr=%trim(%subst(vindta:143:15))
035000010201     C                   EXSR      CHKNUM
035100010201     C                   IF        PiNum=*on
035200011115     C                   Z-ADD     PiVal         VABPKB
035300010201     C                   ELSE
035400010201     C                   ADD       1             errore
035500010201     C                   EVAL      vinmsg = %trimr(vinmsg)
035600010201     C                             + ' ' + 'VABPKB'
035700010201     C                   ENDIF
035800020801     C*
035900020801     C                   EVAL      PiStr=%trim(%subst(vindta:176:15))
036000020801     C                   EXSR      CHKNUM
036100020801     C                   IF        PiNum=*on
036200020801     C* Divido il valore del volume x 10.000 in quanto l'unità misura utilizzata dal cliente è
036300020801     C* il CM cubo mentre noi intendiamo sempre M cubi.
036400020801     C                   EVAL(H)   VABVLB = PiVal/10000
036500020801     C                   ELSE
036600020801     C                   ADD       1             errore
036700020801     C                   EVAL      vinmsg = %trimr(vinmsg)
036800020801     C                             + ' ' + 'VABVLB'
036900020801     C                   ENDIF
037000020619     C*
037100020801     C                   EVAL      PiStr=%trim(%subst(vindta:1:10))
037200010201     C                   EXSR      CHKNUM
037300010201     C                   IF        PiInt=*on
037400011115     C                   Z-ADD     PiVal         VABNSP
037500020305     C                   Z-ADD     PiVal         VATNSP
037600020801     C                   Z-ADD     PiVal         VABRMN
037700010201     C                   ELSE
037800010201     C                   ADD       1             errore
037900010201     C                   EVAL      vinmsg = %trimr(vinmsg)
038000020801     C                             + ' ' + 'VABNSP VATNSP VABRMN'
038100010201     C                   ENDIF
038200020305     C*
038300011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
038400011113     C                   EXSR      CHKIMPDIV
038500010202     C*
038600000801     C* Ebbene...
038700000801     C                   ADD       1             §CTRMO
038800010201     C                   IF        errore <> *zeros
038900000801     C                   ADD       1             §CTRNO
039000000801     C                   EVAL      recko = vindta
039100000801     C                   EXCEPT    rigadett
039200000801     C                   EVAL      vinflg = '2'
039300000801     C                   ELSE
039400010201     C                   ADD       1             §CTROKVB
039500000801     C                   ENDIF
039600000801     C*
039700000801     C                   ENDSR
039800010201     C*----------------------------------------------------*
039900021113     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
040000010201     C*----------------------------------------------------*
040100020305     C     WRIVAT        BEGSR
040200020801     C*
040300020801     C* Reperisco il primo e usltimo numero segnacollo
040400020801     C                   eval      wrkNCDalfa = %subst(vindta:194:4)
040500020801     C                   move(p)   wrkNCDalfa    wrkNCDnum
040600020801     C                   eval      wrkNCAalfa = %subst(vindta:198:4)
040700020801     C                   move(p)   wrkNCAalfa    wrkNCAnum
040800020801     C*
040900020801     C* Ciclo x il numero di colli della spedizione e compongo il barcode
041000020801     C     wrkNCDnum     do        wrkNCAnum     wrkNOTnum
041100020801     C                   move(p)   wrkNOTnum     wrkNOTalfa
041200010201     C*
041300021113     C* Valorizzo l buffer di scrittura del FIVAT
041400020801     C                   EVAL      VATNOT = %subst(vindta:1:10) + wrkNOTalfa
041500021113     C                   write     FIVAT000
041600020801     C                   enddo
041700010201     C*
041800010201     C                   ENDSR
041900010202     C*----------------------------------------------------*
042000021113     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
042100010202     C*----------------------------------------------------*
042200020305     C     PREVAT        BEGSR
042300010202     C*
042400021113     C* Compongo il nome del membro da dare al FIVATWWR
042500010202     C                   eval      parmbr = vlrhdl
042600010202     C                   movel     'M'           parmbr
042700020801     C                   eval      parccm = '00021122'
042800010202     C                   eval      paropz = '1'
042900010202     C* Effettuo la chiamata al CLLE preposto
043000050128     C                   call(e)   'TITVVTC'
043100010202     C                   parm                    parccm
043200010202     C                   parm                    parmbr
043300010202     C                   parm                    paropz
043400010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
043500010202     C                   if        %error
043600010202     C                   movel     '1'           chkcall
043700010202     C                   else
043800010202     C                   movel     '0'           chkcall
043900010202     C                   endif
044000010202     C*
044100010202     C                   ENDSR
044200000801     C*----------------------------------------------------*
044300000801     C*  CONTROLLO NUMERICITA' CAMPI
044400000801     C*----------------------------------------------------*
044500000801     C     CHKNUM        BEGSR
044600000801     C*
044700000801     C                   call(e)   'ISNUMERIC'
044800000801     C                   PARM                    PiStr            30
044900020801     C                   PARM      ','           PiDecChr          1
045000000801     C                   PARM      *ZEROS        PiVal            30 9
045100000801     C                   PARM      '0'           PiInt             1
045200000801     C                   PARM      '0'           PiNum             1
045300000801     C                   IF        %error
045400000801     C                   EVAL      PiInt=*off
045500000801     C                   ENDIF
045600000801     C*
045700000801     C                   ENDSR
045800000801     C***
045900000801
046000011113
046100011113     C*----------------------------------------------------*
046200011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
046300011113     C*----------------------------------------------------*
046400011113     C     CHKIMPDIV     BEGSR
046500011113     C*
046600011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
046700011113     C                   Z-ADD     *zeros        wrkDec            9 9
046800011113     C*
046900011113     C* Come prima cosa effettuo considerazioni sulla divisa
047000011113     C                   IF        vabIAS > *zeros
047100011113     C                   IF        vabVAS <> 'EUR'
047200011113     C                   EVAL      vabVAS =  'ITL'
047300011113     C                   ENDIF
047400011113     C                   ENDIF
047500011113     C*
047600011113     C                   IF        vabCAS > *zeros
047700011113     C                   IF        vabVCA <> 'EUR'
047800011113     C                   EVAL      vabVCA =  'ITL'
047900011113     C                   ENDIF
048000011113     C                   ENDIF
048100011113     C*
048200011113     C                   IF        vabVMD > *zeros
048300020305     C                   IF        vabVAD <> 'EUR'
048400011113     C                   EVAL      vabVAD =  'ITL'
048500011113     C                   ENDIF
048600011113     C                   ENDIF
048700011113     C*
048800011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
048900011113     C                   Z-ADD     vabIAS        wrkDec
049000011113     C                   IF        wrkDec > *zeros
049100011113     C                   IF        vabVAS = 'ITL'
049200011113     C                   EVAL      vabIAS = *zeros
049300011113     C                   ENDIF
049400011113     C                   ENDIF
049500011113     C*
049600011113     C* Stabilisco se il contrasegno ha decimali valorizzati
049700011113     C                   Z-ADD     vabCAS        wrkDec
049800011113     C                   IF        wrkDec > *zeros
049900011113     C                   IF        vabVCA = 'ITL'
050000011113     C                   EVAL      vabCAS = *zeros
050100011113     C                   ENDIF
050200011113     C                   ENDIF
050300011113     C*
050400011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
050500011113     C                   Z-ADD     vabVMD        wrkDec
050600011113     C                   IF        wrkDec > *zeros
050700011113     C                   IF        vabVAD = 'ITL'
050800011113     C                   EVAL      vabVMD = *zeros
050900011113     C                   ENDIF
051000011113     C                   ENDIF
051100011113     C*
051200011113     C                   ENDSR
051300011113     C***
051400011113
051500011113
051600000801
051700000801
051800990920      /TITLE Invio dei dati al punto operativo.
051900010202     C     invio         BEGSR
052000990920     C*
052100021113     C* 1° invio FIVAT
052200010201     C                   reset                   dscmz
052300010201     C                   move      vlrpoi        cmzdst
052400021113     C                   eval      cmzfld = 'FIVATWWR'
052500010201     C                   eval      cmzmbd = vlrhdl
052600010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
052700021009     C***                if        prmfir = *blanks
052800021113     C                   eval      cmzfla = 'FIVAT00F'
052900021113     C                   eval      cmzmba = 'FIVAT00F'
053000021009     C***                else
053100021009     C***                eval      cmzfla = prmfir
053200021009     C***                eval      cmzmba = prmfir
053300021009     C***                endif
053400010201     C                   eval      cmznrr = *zeros
053500020305     C                   move      §ctrokvt      cmznrr
053600021018     C                   eval      cmzlba = vlrfl1
053700010201     C                   call(e)   'TIS711C'
053800010201     C                   parm                    dscmz
053900010201     C                   parm      *blanks       esito
054000010205     C                   if        %error
054100010205     C                             or cmzerr = '1'
054200010205     C                             or esito  = '1'
054300010205     C                   eval      wrkesito = '3'
054400010205     C                   else
054500010201     C*
054600021113     C* 2° invio FIVAB
054700010201     C                   reset                   dscmz
054800010201     C                   move      vlrpoi        cmzdst
054900010201     C                   eval      cmzfld = vlrfou
055000010201     C                   eval      cmzmbd = vlrhdl
055100010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
055200021009     C***                if        prmfir = *blanks
055300021113     C                   eval      cmzfla = 'FIVAB00F'
055400021113     C                   eval      cmzmba = 'FIVAB00F'
055500021009     C***                else
055600021009     C***                eval      cmzfla = prmfir
055700021009     C***                eval      cmzmba = prmfir
055800021009     C***                endif
055900010201     C                   eval      cmznrr = *zeros
056000010201     C                   move      §ctrokvb      cmznrr
056100021018     C                   eval      cmzlba = vlrfl1
056200010201     C                   call(e)   'TIS711C'
056300010201     C                   parm                    dscmz
056400010201     C                   parm      *blanks       esito
056500010201     C                   if        %error
056600010201     C                             or cmzerr = '1'
056700010201     C                             or esito  = '1'
056800010201     C                   eval      wrkesito = '3'
056900010201     C                   endif
057000010205     C                   endif
057100990920     C*
057200000613     C                   ENDSR
057300000613     C***
057400070411
057500070411     C     *pssr         BEGSR
057600070411     C*
057700070411     C                   if        %open(tivin00r)
057800070411     C                   close     tivin00r
057900070411     C                   endif
058000070411     C                   if        %open(fivabwwr)
058100070411     C                   close     fivabwwr
058200070411     C                   endif
058300070411     C                   if        %open(fivatwwr)
058400070411     C                   close     fivatwwr
058500070411     C                   endif
058600070411     C*
058700070411     C* Effettuo la chiamata al CLLE preposto
058800070411     C                   call(e)   'TITVVTC'
058900070411     C                   parm                    parccm
059000070411     C                   parm                    parmbr
059100070411     C                   parm      '2'           paropz
059200070411     C*
059300070411     C                   eval      wrkesito = '2'
059400070411     C*
059500070411     C                   seton                                        LR
059600070411     C*
059700070411     C                   ENDSR     '*CANCL'
059800070411     C***
059900070411
060000990910
060100000613     C     *inzsr        BEGSR
060200990910     C*
060300990910     C     *entry        plist
060400990920     C                   parm                    tivlrds
060500990921     C                   parm      wrkesito      esito
060600000724     C                   parm                    prmlit
060700000710     C                   parm                    prmfir
060800000613     C*
060900000830     C* CALCOLA LA DATA CORRENTE
061000000830     C                   time                    wn14             14 0
061100000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
061200000830     C                   z-add     wn8           g08dat
061300000830     C                   z-add     *zeros        g08inv
061400000830     C                   movel     '0'           g08err
061500000830     C                   call      'XSRDA8'
061600000830     C                   parm                    wlbda8
061700000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
061800000830     C*
061900000613     C                   ENDSR
062000000613     C***
062100990908
062200020801     Otitv21p   E            riepilogo         2
062300990915     O                                              'Upload via Internet'
062400990915     O                                           +1 'Traduzione TIVIN00R -'
062500021113     O                                           +1 'FIVABWWR/FIVATWWR'
062600010201     O                                           +1 'Report testate bolle'
062700990915     O          E            riepilogo   2
062800990915     O                       wrkdata
062900990915     O                       wrkora              +1
063000990915     O                       procname            +1
063100990915     O          E            riepilogo   2
063200990915     O                                              'Cliente..................:'
063300990915     O                       VABCCM        z     +1
063400990915     O          E            riepilogo   2
063500990920     O                                              'Riferimento Strategi.....:'
063600990920     O                       vlrhdl              +1
063700990915     O          E            riepilogo   2
063800990915     O                                              'Giusti...................:'
063900010201     O                       §CTROKVB      2   +  1
064000990915     O          E            riepilogo   2
064100990915     O                                              'Sbagliati e corretti.....:'
064200971022     O                       §CTRMO        2   +  1
064300990915     O          E            riepilogo   2
064400990915     O                                              'Sbagliati e scartati.....:'
064500971022     O                       §CTRNO        2   +  1
064600000613
064700020801     Otitv21ps  E            testdett          2
064800000613     O                                              'Upload via Internet'
064900000613     O                                           +1 'Traduzione TIVIN00R -'
065000021113     O                                           +1 'FIVABWWR/FIVATWWR'
065100010201     O                                           +1 'Report testate bolle'
065200000616     O          E            testdett    3
065300000613     O                                           +2 'N° rec'
065400000613     O                                           +3 'Anteprima contenuto'
065500000616     O          E            rigadett    2
065600000613     O                       rrnum               +2
065700000621     O                       recko               +3
065800000616     O          E            findett     2
065900000613     O                       wrkdata
066000000613     O                       wrkora              +1
066100000613     O                       procname            +1
066200000616     O          E            findett     2
066300000613     O                                              'Cliente..................:'
066400000613     O                       VABCCM        z     +1
066500000616     O          E            findett     2
066600000613     O                                              'Riferimento Strategi.....:'
066700000613     O                       vlrhdl              +1
066800000616     O          E            findett     2
066900000613     O                                              'Giusti...................:'
067000010201     O                       §CTROKVB      2   +  1
067100000616     O          E            findett     2
067200000613     O                                              'Sbagliati e corretti.....:'
067300000613     O                       §CTRMO        2   +  1
067400000616     O          E            findett     2
067500000613     O                                              'Sbagliati e scartati.....:'
067600000613     O                       §CTRNO        2   +  1
067700000512** CMD - COMANDI CL
067800020801OVRPRTF FILE(TITV21P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
067900020801OVRPRTF FILE(TITV21PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
068000020801DLTOVR FILE(TITV21P TITV21PS) LVL(*)
068100000512
068200000512
