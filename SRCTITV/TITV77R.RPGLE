000100021017      /TITLE Upload via Internet: traduzione in FIVABWWR/FIVATWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600021017     FFIVABwwr  O    E             DISK    usropn
000700021017     FFIVATwwr  O    E             DISK    usropn
000800031127     Ftitv77p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100031127     Ftitv77ps  O    f  198        PRINTER usropn
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
003500030415     D depspe          s              7    INZ(*blanks)
003600030415     D curspe          s              7    INZ(*blanks)
003700030415     D cntspe          s              7  0 INZ(*zeros)
003800010202     D parccm          s              8    INZ(*blanks)
003900010202     D parmbr          s             10    INZ(*blanks)
004000010202     D paropz          s              1    INZ(*blanks)
004100010202     D chkcall         s              1    INZ(*blanks)
004200010213
004300000801     D  Num5_0         s              5  0
004400030107     D  Num7_0         s              7  0
004500000830
004600000830     D*------------------
004700000830     D* DS "XSRDA8" - CONTROLLA DATA (8)
004800000830     D*------------------
004900000830     D WLBDA8          DS                  INZ
005000000830     D  G08DAT                 1      8  0
005100000830     D  G08INV                 9     16  0
005200000830     D  G08ERR                17     17
005300000830     D  G08TGI                18     22  0
005400031127     D*------------------
005500031127     D* DS REPERIMENTO NUMERATORE
005600031127     D*------------------
005700031127     D trul33ds      e ds                  inz
005800031127     D*------------------
005900031127     D* DS ARCHITETTURA
006000031127     D*------------------
006100031127     D kpjba         e ds                  inz
006200990908
006300010201
006400010201
006500990915     C                   time                    wrkdata
006600990915     C                   time                    wrkora
006700000913     C                   reset                   rrnum
006800990921     C                   reset                   esito
006900990921     C                   reset                   wrkesito
007000000724     C*
007100000724     C* SE OCCORRE SPEDIRE IN FILIALE
007200021017     C                   if        vlrpoi <> *zeros
007300000724     C*
007400000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
007500000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
007600021017     C     vlrpoi        chain     azorg01l
007700000724     C                   if        %found
007800000616     C                   movel(p)  CMD(1)        depcmd
007900020809     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
008000000616     C*
008100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
008200011113     C                   Z-ADD     150           LENGH            15 5
008300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
008400000616     C                   PARM                    depcmd
008500000616     C                   PARM                    LENGH
008600000724     C*
008700000724     C                   endif
008800000724     C                   endif
008900000616     C*
009000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009100000616     C                   movel(p)  CMD(2)        depcmd
009200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
009300000616     C*
009400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
009500011113     C                   Z-ADD     150           LENGH            15 5
009600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
009700000616     C                   PARM                    depcmd
009800000616     C                   PARM                    LENGH
009900000616     C*
010000031127     C                   if        not %open(titv77ps)
010100031127     C                   open      titv77ps
010200000616     C                   except    testdett
010300000613     C                   endif
010400000613     C*
010500030226     C                   EXSR      RWFILE                                       LETT/SCR. VAB
010600000621     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
010700000613     C*
010800010202     C* Effettuo la chiamata al CLLE preposto
010900050128     C                   call(e)   'TITVVTC'
011000010202     C                   parm                    parccm
011100010202     C                   parm                    parmbr
011200010202     C                   parm      '2'           paropz
011300010202     C*
011400031127     C                   if        %open(titv77ps)
011500000616     C                   except    findett
011600031127     C                   close     titv77ps
011700000613     C                   endif
011800000616     C*
011900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
012000011113     C                   Z-ADD     150           LENGH            15 5
012100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012200000616     C                   PARM                    CMD(3)
012300000616     C                   PARM                    LENGH
012400000616     C*
012500010201     C                   seton                                        LR
012600000613
012700000613
012800000613     C*--------------------------------------------------------
012900000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
013000000613     C*--------------------------------------------------------
013100000621     C     STPR          BEGSR
013200000613     C*
013300031127     C                   if        not %open(titv77p)
013400031127     C                   open      titv77p
013500990915     C                   endif
013600990915     C*
013700990915     C                   except    riepilogo
013800990915     C*
013900031127     C                   if        %open(titv77p)
014000031127     C                   close     titv77p
014100990914     C                   endif
014200990910     C*
014300000613     C                   ENDSR
014400000613     C***
014500990908
014600000801
014700910830     C*--------------------------------------------------------
014800030226     C* RWFILE LEGGE tivin00r E SCRIVE FIVABWWR e FIVATWWR    *
014900910830     C*--------------------------------------------------------
015000030226     C     RWFILE        BEGSR
015100990910     C*
015200990914     C                   if        not %open(tivin00r)
015300990908     C                   open      tivin00r
015400990914     C                   endif
015500021017     C                   if        not %open(fivabwwr)
015600021017     C                   open      fivabwwr
015700990914     C                   endif
015800021017     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
015900020305     C                   exsr      prevat
016000010201     C*
016100010202     C                   if        chkcall = '0'
016200010202     C*
016300021017     C                   if        not %open(fivatwwr)
016400021017     C                   open      fivatwwr
016500010201     C                   endif
016600990910     C*
016700010201     C                   clear                   §CTROKVB          5 0
016800020305     C                   clear                   §CTROKVT          5 0
016900000801     C                   clear                   §CTRMO            5 0
017000000801     C                   clear                   §CTRNO            5 0
017100990910     C*
017200921023     C                   DO        *HIVAL
017300990913     C*
017400990915     C                   READ      tivin00r                               70
017500000905     C                   if        vindta > *blanks
017600000613     C                   add       1             rrnum
017700000801     C*
017800000801     C                   if        *in70 = *off
017900000801     C                             and
018000000801     C                             (vinflg = *blanks
018100000801     C                              or vinflg = '0'
018200000801     C                              or vinflg = '2')
018300000801     C*
018400000801     C                   clear                   vinmsg
018500030310     C                   eval      vinflg = '1'
018600030411     C*
018700030411     C                   clear                   fivab000
018800030411     C*
018900030411     C                   exsr      impvab                                       => carico VAB
019000030411     C                   exsr      impvat                                       => carico VAT
019100030411     C                   exsr      wrivab                                       => scarico VAB
019200030411     C                   exsr      wrivat                                       => scarico VAT
019300000905     C*
019400000905     C                   else
019500000905     C                   eval      vinflg = '1'
019600000905     C                   endif
019700030411     C                   endif
019800000905     C*
019900000905     C  N70              update    tivin000
020000000905     C*
020100991022     C  N70              ENDdo
020200010202     C*
020300010202     C                   endif
020400990910
020500990910     C* Se non ci sono record con errori ...
020600000710     C                   if        §ctrno = 0
020700990910     C* ... restituisco esito OK.
020800990921     C                   eval      wrkesito = '0'
020900990910     C                   else
021000010201     C                   if        §ctrokvb > 0
021100990921     C                   eval      wrkesito = '1'
021200000710     C                   else
021300000710     C                   eval      wrkesito = '2'
021400990910     C                   endif
021500000710     C                   endif
021600990910     C*
021700990914     C                   if        %open(tivin00r)
021800990908     C                   close     tivin00r
021900990914     C                   endif
022000021017     C                   if        %open(fivabwwr)
022100021017     C                   close     fivabwwr
022200990914     C                   endif
022300021017     C                   if        %open(fivatwwr)
022400021017     C                   close     fivatwwr
022500010201     C                   endif
022600021017     C*
022700021017     C                   if        vlrpoi <> 999
022800021017     C                   eval      vlrpoi = vlrpoi
022900021017     C                   endif
023000990910     C*
023100010201     C                   if        §ctrokvb > 0
023200021017     C                             and vlrpoi > *zeros
023300010202     C                   exsr      invio
023400990920     C                   endif
023500990920     C*
023600910830     C                   ENDSR
023700000613     C***
023800990920
023900000801     C*----------------------------------------------------*
024000000801     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
024100000801     C*----------------------------------------------------*
024200010201     C     INZVAR        BEGSR
024300000801     C*
024400010201     C                   Z-ADD     *zeros        Num5_0
024500030107     C                   Z-ADD     *zeros        Num7_0
024600030310     C* Inizializzo 1 flag che uso x considerazioni particolari su CBO/CAS
024700030310     C                   MOVEL     '0'           FlgCAS            1
024800000801     C*
024900000801     C                   ENDSR
025000000801     C*----------------------------------------------------*
025100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
025200000801     C*----------------------------------------------------*
025300000801     C     DEFCAM        BEGSR
025400000801     C*
025500020619     C* Imposto i valori di default...
025600030415     C                   Z-ADD     0492593       VABCCM
025700030415     C                   Z-ADD     0492593       VATCCM
025800021017     C                   Z-ADD     vlrpoi        VABFGS
025900021017     C                   Z-ADD     vlrpoi        VATFGS
026000030415     C                   Z-ADD     049           VABLNP
026100030415     C                   Z-ADD     049           VATLNP
026200040407     C                   Z-ADD     100           VABCTR
026300021017     C                   MOVEL     '7Q'          VABCTM
026400020305     C                   MOVEL     'E'           VATTRC
026500020619     C* ... e poi verifico se sono stati passati come parametri
026600020619     C                   IF        vlrppt > *blanks
026700020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:1:7))
026800020619     C                   EXSR      CHKNUM
026900020619     C                   IF        PiInt=*on
027000020619     C                   Z-ADD     PiVal         VABCCM
027100020619     C                   Z-ADD     PiVal         VATCCM
027200020619     C                   ENDIF
027300020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
027400020619     C                   EXSR      CHKNUM
027500020619     C                   IF        PiInt=*on
027600020619     C                   Z-ADD     PiVal         VABLNP
027700020619     C                   Z-ADD     PiVal         VATLNP
027800020619     C                   ENDIF
027900020619     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
028000020619     C                   EXSR      CHKNUM
028100020619     C                   IF        PiInt=*on
028200020619     C                   Z-ADD     PiVal         VABCTR
028300020619     C                   ENDIF
028400020619     C                   ENDIF
028500000801     C*
028600000801     C                   ENDSR
028700000801     C*----------------------------------------------------*
028800030415     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAB)
028900000801     C*----------------------------------------------------*
029000010201     C     IMPVAB        BEGSR
029100000801     C*
029200020305     C                   EXSR      INZVAR
029300020305     C                   EXSR      DEFCAM
029400010305     C*
029500030226     C                   Z-ADD     *zeros        errore            1 0
029600000830     C                   MOVEL     datcor        VABAAS
029700000830     C                   MOVE      datcor        VABMGS
029800030226     C*
029900030415     C                   EVAL      VABRSD=%trim(%subst(vindta:32:30))
030000020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
030100020117     C     '@':'A'       XLATE     VABRSD        VABRSD
030200020117     C* ==
030300030415     C                   IF        %subst(vindta:62:3) = 'IT'
030400030415     C                   ELSE
030500030415     C                   EVAL      VABNZD=%trim(%subst(vindta:62:3))
030600030415     C                   ENDIF
030700030415     C                   EVAL      VABIND=%trim(%subst(vindta:65:30))
030800030415     C                   EVAL      VABLOD=%trim(%subst(vindta:155:30))
030900030415     C                   EVAL      VABPRD=%trim(%subst(vindta:95:30))
031000031127     C***                EVAL      VABRMA=%trim(%subst(vindta:22:10))
031100031127     C                   EVAL      VABRMA=%trim(%subst(vindta:14:8))
031200030311     C                   EVAL      VABVCA='EUR'
031300030226     C* CAP
031400030415     C                   EVAL      PiStr=%trim(%subst(vindta:185:10))
031500010201     C                   EXSR      CHKNUM
031600010201     C                   IF        PiInt=*on
031700010201     C                   Z-ADD     PiVal         Num5_0
031800021017     C                   MOVEL     Num5_0        VABCAD
031900010201     C                   ELSE
032000031127     C                   Z-ADD     1             errore
032100010201     C                   EVAL      vinmsg = %trimr(vinmsg)
032200021017     C                             + ' ' + 'VABCAD'
032300010201     C                   ENDIF
032400031127     C* NSP => Stacco un numeratore da AZNUM
032500031127     C                   clear                   TRUL33DS
032600031127     C                   eval      I33OPE = *zeros
032700031127     C                   eval      I33CNU = 302
032800031127     C                   eval      I33NUM = 1
032900031127     C                   movel     TRUL33DS      KPJBU
033000031127     C                   call      'TRUL33R'
033100031127     C                   parm                    KPJBA
033200031127     C                   movel     KPJBU         TRUL33DS
033300031127     C                   if        O33ERR = *zeros
033400031127     C                   z-add     O33NRF        VABNSP
033500031127     C                   z-add     O33NRF        VATNSP
033600031127     C                   else
033700031127     C                   Z-ADD     1             errore
033800031127     C                   EVAL      vinmsg = %trimr(vinmsg)
033900031127     C                             + ' ' + 'VABNSP VATNSP'
034000031127     C                   endif
034100030314     C* RMN
034200030415     C                   EVAL      PiStr=%trim(%subst(vindta:1:13))
034300030311     C                   EXSR      CHKNUM
034400030311     C                   IF        PiInt=*on
034500030311     C                   Z-ADD     PiVal         VABRMN
034600030311     C                   ELSE
034700031127     C                   Z-ADD     1             errore
034800030311     C                   EVAL      vinmsg = %trimr(vinmsg)
034900030311     C                             + ' ' + 'VABRMN'
035000030311     C                   ENDIF
035100030226     C* NCL
035200031127     C                   EVAL      VABNCL = 1
035300030226     C* PKB
035400030415     C                   EVAL      PiStr=%trim(%subst(vindta:203:7))
035500030107     C                   EXSR      CHKNUM
035600030107     C                   IF        PiNum=*on
035700031127     C                   Z-ADD     PiVal         VABPKB
035800030107     C                   ELSE
035900031127     C                   Z-ADD     1             errore
036000030107     C                   EVAL      vinmsg = %trimr(vinmsg)
036100030107     C                             + ' ' + 'VABPKB'
036200030107     C                   ENDIF
036300030415     C* CAS
036400030415     C                   IF        %subst(vindta:210:13) <> '0000000000,00'
036500030310     C                   MOVEL     '1'           FlgCAS
036600030804     C                   EVAL      VABTIC = 'OM'
036700030804     C                   EVAL      VABRMO = 'BELGARDA SPA'
036800030804     C                   EVAL      VABCMO = '20050'
036900030415     C                   EVAL      PiStr=%trim(%subst(vindta:210:13))
037000030310     C                   EXSR      CHKNUM
037100030310     C                   IF        PiNum=*on
037200031127     C                   Z-ADD     PiVal         VABCAS
037300030310     C                   ELSE
037400030310     C                   SETON                                        32
037500030310     C                   Z-ADD     *zeros        VABCAS
037600030310     C                   EVAL      vinmsg = %trimr(vinmsg)
037700030310     C                             + ' ' + 'VABCAS'
037800030310     C                   ENDIF
037900030310     C                   ENDIF
038000030310     C*
038100030310     C* Considerazioni finali su CBO/CAS
038200030310     C                   IF        FlgCAS = '1'
038300010205     C                   EVAL      VABCBO = '4'
038400010205     C                   ELSE
038500010205     C                   EVAL      VABCBO = '1'
038600010205     C                   ENDIF
038700020305     C*
038800011113     C* Eseguo routine finale x considerazioni specifiche su importi/divise
038900011113     C                   EXSR      CHKIMPDIV
039000010202     C*
039100000801     C* Ebbene...
039200000801     C                   ADD       1             §CTRMO
039300010201     C                   IF        errore <> *zeros
039400000801     C                   ADD       1             §CTRNO
039500000801     C                   EVAL      recko = vindta
039600000801     C                   EXCEPT    rigadett
039700000801     C                   EVAL      vinflg = '2'
039800000801     C                   ELSE
039900010201     C                   ADD       1             §CTROKVB
040000000801     C                   ENDIF
040100000801     C*
040200000801     C                   ENDSR
040300030226     C*----------------------------------------------------*
040400030226     C*  SCARICAMENTO BUFFER RECORDS VAB
040500030226     C*----------------------------------------------------*
040600030226     C     WRIVAB        BEGSR
040700030310     C*
040800030310     C                   write     FIVAB000
040900030226     C*
041000030226     C                   ENDSR
041100030226     C*----------------------------------------------------*
041200030226     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
041300030226     C*----------------------------------------------------*
041400030226     C     IMPVAT        BEGSR
041500030226     C*
041600030226     C                   EXSR      INZVAR
041700030226     C                   EXSR      DEFCAM
041800030226     C*
041900030226     C                   MOVEL     datcor        VATAAS
042000030226     C*
042100030415     C* Valorizzo il barcode
042200030415     C                   EVAL      VATNOT=%trim(%subst(vindta:14:8))
042300030226     C*
042400030226     C                   ENDSR
042500010201     C*----------------------------------------------------*
042600021017     C*  CONSIDERAZIONI E SCRITTURA RECORD DI DETTAGLIO DA FLAT FILE (X FIVAT)
042700010201     C*----------------------------------------------------*
042800020305     C     WRIVAT        BEGSR
042900010201     C*
043000021017     C                   write     FIVAT000
043100010201     C*
043200010201     C                   ENDSR
043300010202     C*----------------------------------------------------*
043400021017     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
043500010202     C*----------------------------------------------------*
043600020305     C     PREVAT        BEGSR
043700010202     C*
043800021017     C* Compongo il nome del membro da dare al FIVATWWR
043900010202     C                   eval      parmbr = vlrhdl
044000010202     C                   movel     'M'           parmbr
044100030415     C                   eval      parccm = '00492593'
044200010202     C                   eval      paropz = '1'
044300010202     C* Effettuo la chiamata al CLLE preposto
044400050128     C                   call(e)   'TITVVTC'
044500010202     C                   parm                    parccm
044600010202     C                   parm                    parmbr
044700010202     C                   parm                    paropz
044800010202     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
044900010202     C                   if        %error
045000010202     C                   movel     '1'           chkcall
045100010202     C                   else
045200010202     C                   movel     '0'           chkcall
045300010202     C                   endif
045400010202     C*
045500010202     C                   ENDSR
045600000801     C*----------------------------------------------------*
045700000801     C*  CONTROLLO NUMERICITA' CAMPI
045800000801     C*----------------------------------------------------*
045900000801     C     CHKNUM        BEGSR
046000000801     C*
046100000801     C                   call(e)   'ISNUMERIC'
046200000801     C                   PARM                    PiStr            30
046300030107     C                   PARM      ','           PiDecChr          1
046400000801     C                   PARM      *ZEROS        PiVal            30 9
046500000801     C                   PARM      '0'           PiInt             1
046600000801     C                   PARM      '0'           PiNum             1
046700000801     C                   IF        %error
046800000801     C                   EVAL      PiInt=*off
046900030107     C                   EVAL      PiNum=*off
047000000801     C                   ENDIF
047100000801     C*
047200000801     C                   ENDSR
047300000801     C***
047400000801
047500011113
047600011113     C*----------------------------------------------------*
047700011113     C*  CONSIDERAZIONI SU IMPORTI/DIVISE
047800011113     C*----------------------------------------------------*
047900011113     C     CHKIMPDIV     BEGSR
048000011113     C*
048100011113     C* Definisco ed inizializzo qui le variabili di wrk x una maggiore flessibilità
048200011113     C                   Z-ADD     *zeros        wrkDec            9 9
048300011113     C*
048400011113     C* Come prima cosa effettuo considerazioni sulla divisa
048500011113     C                   IF        vabIAS > *zeros
048600011113     C                   IF        vabVAS <> 'EUR'
048700011113     C                   EVAL      vabVAS =  'ITL'
048800011113     C                   ENDIF
048900011113     C                   ENDIF
049000011113     C*
049100011113     C                   IF        vabCAS > *zeros
049200011113     C                   IF        vabVCA <> 'EUR'
049300011113     C                   EVAL      vabVCA =  'ITL'
049400011113     C                   ENDIF
049500011113     C                   ENDIF
049600011113     C*
049700011113     C                   IF        vabVMD > *zeros
049800020305     C                   IF        vabVAD <> 'EUR'
049900011113     C                   EVAL      vabVAD =  'ITL'
050000011113     C                   ENDIF
050100011113     C                   ENDIF
050200011113     C*
050300011113     C* Stabilisco se l'importo da assicurare ha decimali valorizzati
050400011113     C                   Z-ADD     vabIAS        wrkDec
050500011113     C                   IF        wrkDec > *zeros
050600011113     C                   IF        vabVAS = 'ITL'
050700011113     C                   EVAL      vabIAS = *zeros
050800011113     C                   ENDIF
050900011113     C                   ENDIF
051000011113     C*
051100011113     C* Stabilisco se il contrasegno ha decimali valorizzati
051200011113     C                   Z-ADD     vabCAS        wrkDec
051300011113     C                   IF        wrkDec > *zeros
051400011113     C                   IF        vabVCA = 'ITL'
051500011113     C                   EVAL      vabCAS = *zeros
051600011113     C                   ENDIF
051700011113     C                   ENDIF
051800011113     C*
051900011113     C* Stabilisco se il valore merce dichiarato ha decimali valorizzati
052000011113     C                   Z-ADD     vabVMD        wrkDec
052100011113     C                   IF        wrkDec > *zeros
052200011113     C                   IF        vabVAD = 'ITL'
052300011113     C                   EVAL      vabVMD = *zeros
052400011113     C                   ENDIF
052500011113     C                   ENDIF
052600011113     C*
052700011113     C                   ENDSR
052800011113     C***
052900021017
053000000801
053100000801
053200990920      /TITLE Invio dei dati al punto operativo.
053300010202     C     invio         BEGSR
053400990920     C*
053500021017     C* 1° invio FIVAT
053600010201     C                   reset                   dscmz
053700021017     C                   move      vlrpoi        cmzdst
053800021017     C                   eval      cmzfld = 'FIVATWWR'
053900010201     C                   eval      cmzmbd = vlrhdl
054000010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
054100021009     C***                if        prmfir = *blanks
054200021017     C                   eval      cmzfla = 'FIVAT00F'
054300021017     C                   eval      cmzmba = 'FIVAT00F'
054400021009     C***                else
054500021009     C***                eval      cmzfla = prmfir
054600021009     C***                eval      cmzmba = prmfir
054700021009     C***                endif
054800010201     C                   eval      cmznrr = *zeros
054900020305     C                   move      §ctrokvt      cmznrr
055000021018     C                   eval      cmzlba = vlrfl1
055100010201     C                   call(e)   'TIS711C'
055200010201     C                   parm                    dscmz
055300010201     C                   parm      *blanks       esito
055400010205     C                   if        %error
055500010205     C                             or cmzerr = '1'
055600010205     C                             or esito  = '1'
055700010205     C                   eval      wrkesito = '3'
055800010205     C                   else
055900010201     C*
056000021017     C* 2° invio FIVAB
056100010201     C                   reset                   dscmz
056200021017     C                   move      vlrpoi        cmzdst
056300010201     C                   eval      cmzfld = vlrfou
056400010201     C                   eval      cmzmbd = vlrhdl
056500010201     C                   eval      %subst(cmzmbd:1:1) = 'M'
056600021009     C***                if        prmfir = *blanks
056700021017     C                   eval      cmzfla = 'FIVAB00F'
056800021017     C                   eval      cmzmba = 'FIVAB00F'
056900021009     C***                else
057000021009     C***                eval      cmzfla = prmfir
057100021009     C***                eval      cmzmba = prmfir
057200021009     C***                endif
057300010201     C                   eval      cmznrr = *zeros
057400010201     C                   move      §ctrokvb      cmznrr
057500021018     C                   eval      cmzlba = vlrfl1
057600010201     C                   call(e)   'TIS711C'
057700010201     C                   parm                    dscmz
057800010201     C                   parm      *blanks       esito
057900010201     C                   if        %error
058000010201     C                             or cmzerr = '1'
058100010201     C                             or esito  = '1'
058200010201     C                   eval      wrkesito = '3'
058300010201     C                   endif
058400010205     C                   endif
058500990920     C*
058600000613     C                   ENDSR
058700000613     C***
058800070411
058900070411     C     *pssr         BEGSR
059000070411     C*
059100070411     C                   if        %open(tivin00r)
059200070411     C                   close     tivin00r
059300070411     C                   endif
059400070411     C                   if        %open(fivabwwr)
059500070411     C                   close     fivabwwr
059600070411     C                   endif
059700070411     C                   if        %open(fivatwwr)
059800070411     C                   close     fivatwwr
059900070411     C                   endif
060000070411     C*
060100070411     C* Effettuo la chiamata al CLLE preposto
060200070411     C                   call(e)   'TITVVTC'
060300070411     C                   parm                    parccm
060400070411     C                   parm                    parmbr
060500070411     C                   parm      '2'           paropz
060600070411     C*
060700070411     C                   eval      wrkesito = '2'
060800070411     C*
060900070411     C                   seton                                        LR
061000070411     C*
061100070411     C                   ENDSR     '*CANCL'
061200070411     C***
061300070411
061400990910
061500000613     C     *inzsr        BEGSR
061600990910     C*
061700990910     C     *entry        plist
061800990920     C                   parm                    tivlrds
061900990921     C                   parm      wrkesito      esito
062000000724     C                   parm                    prmlit
062100000710     C                   parm                    prmfir
062200000613     C*
062300000830     C* CALCOLA LA DATA CORRENTE
062400000830     C                   time                    wn14             14 0
062500000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
062600000830     C                   z-add     wn8           g08dat
062700000830     C                   z-add     *zeros        g08inv
062800000830     C                   movel     '0'           g08err
062900000830     C                   call      'XSRDA8'
063000000830     C                   parm                    wlbda8
063100000830     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
063200000830     C*
063300000613     C                   ENDSR
063400000613     C***
063500990908
063600031127     Otitv77p   E            riepilogo         2
063700990915     O                                              'Upload via Internet'
063800990915     O                                           +1 'Traduzione TIVIN00R -'
063900021017     O                                           +1 'FIVABWWR/FIVATWWR'
064000010201     O                                           +1 'Report testate bolle'
064100990915     O          E            riepilogo   2
064200990915     O                       wrkdata
064300990915     O                       wrkora              +1
064400990915     O                       procname            +1
064500990915     O          E            riepilogo   2
064600990915     O                                              'Cliente..................:'
064700990915     O                       VABCCM        z     +1
064800990915     O          E            riepilogo   2
064900990920     O                                              'Riferimento Strategi.....:'
065000990920     O                       vlrhdl              +1
065100990915     O          E            riepilogo   2
065200990915     O                                              'Giusti...................:'
065300010201     O                       §CTROKVB      2   +  1
065400990915     O          E            riepilogo   2
065500990915     O                                              'Sbagliati e corretti.....:'
065600971022     O                       §CTRMO        2   +  1
065700990915     O          E            riepilogo   2
065800990915     O                                              'Sbagliati e scartati.....:'
065900971022     O                       §CTRNO        2   +  1
066000000613
066100031127     Otitv77ps  E            testdett          2
066200000613     O                                              'Upload via Internet'
066300000613     O                                           +1 'Traduzione TIVIN00R -'
066400021017     O                                           +1 'FIVABWWR/FIVATWWR'
066500010201     O                                           +1 'Report testate bolle'
066600000616     O          E            testdett    3
066700000613     O                                           +2 'N° rec'
066800000613     O                                           +3 'Anteprima contenuto'
066900000616     O          E            rigadett    2
067000000613     O                       rrnum               +2
067100000621     O                       recko               +3
067200000616     O          E            findett     2
067300000613     O                       wrkdata
067400000613     O                       wrkora              +1
067500000613     O                       procname            +1
067600000616     O          E            findett     2
067700000613     O                                              'Cliente..................:'
067800000613     O                       VABCCM        z     +1
067900000616     O          E            findett     2
068000000613     O                                              'Riferimento Strategi.....:'
068100000613     O                       vlrhdl              +1
068200000616     O          E            findett     2
068300000613     O                                              'Giusti...................:'
068400010201     O                       §CTROKVB      2   +  1
068500000616     O          E            findett     2
068600000613     O                                              'Sbagliati e corretti.....:'
068700000613     O                       §CTRMO        2   +  1
068800000616     O          E            findett     2
068900000613     O                                              'Sbagliati e scartati.....:'
069000000613     O                       §CTRNO        2   +  1
069100000512** CMD - COMANDI CL
069200031127OVRPRTF FILE(TITV77P) TOFILE(TIS7T7P) OVRSCOPE(*CALLLVL)   FORMTYPE(RICCLI) OUTQ(
069300031127OVRPRTF FILE(TITV77PS) TOFILE(TIS7T7PS) OVRSCOPE(*CALLLVL) FORMTYPE(RICCLI) OUTQ(
069400031127DLTOVR FILE(TITV77P TITV77PS) LVL(*)
069500000512
069600000512
