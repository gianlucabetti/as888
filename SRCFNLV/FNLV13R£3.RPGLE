000100120319     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio) BNDDIR('TIS')
000200970703     H* FNLV13R  *---------------------------------------------------*
000300970703     H*     CONTROLLO CAP
000400921021     H*--------------------------------------------------------------*
000500970708     H* CONPORTAMENTI DEL TISI95R
000600970708     H* 1) SE TIPO CONTROLLO 7:
000700970708     H*   *CHIEDE GIA' DA SOLO LA PROVINCIA
000800970708     H*                         OBBLIGATORIA PER ITALIA (NON PER 3 E 4)
000900970708     H*   *PUO' RESTITUIRE I SEGUENTI DATI MODIFICATI IN AUTOMATICO :
001000970708     H*         CAP  NAZIONE  E  PROVINCIA
001100970708     H*         EMETTERE MSG DI AVVISO EVIDENZIANDO I DATI
001200970708     H* 2) PER ANDARE IN INTERROGAIZONE CAPPARIO IMMETTERE LA LOCALITA'
001300970708     H*    SENZA LA PROVINCIA E IL CAP
001400970708     H*--------------------------------------------------------------*
001500080414     D ERR             S             78    DIM(19) CTDATA PERRCD(1)
001600970701     D* DS PER TISI95R - CONTROLLO DI UN CAP
001700970701     D DSSI95        E DS                  EXTNAME(TISI95DS)
001800970707     D DSSI00        E DS                  EXTNAME(TISI00DS)
001900970707     D DSLV13        E DS                  EXTNAME(FNLV13DS)
002000060921     D TISI97DS      E DS
002100061030     D dcppflo       E DS
002200970707     D KPJBA         E DS
002300000721     D                SDS
002400000721     D  PARMS                 37     39  0
002500120319     ***************************************************************************
002600120319     D rtvMsgLang      PR          3512A
002700120319     D  msgId                         7A   CONST
002800120322     D  pilinguaISO2                  2A   OPTIONS(*OMIT:*NOPASS)
002900120319     D  piMsgDta                    512A   OPTIONS(*OMIT:*NOPASS:*VARSIZE) CONST
003000120319     D  piMsg                       644A   OPTIONS(*OMIT:*NOPASS)
003100120319     D                                     VARYING
003200120319     D  piSecLvl                   3512A   OPTIONS(*OMIT:*NOPASS)
003300120319     D                                     VARYING
003400120319     D  piRtnCode                    10A   OPTIONS(*OMIT:*NOPASS)
003500120319     D  piEsito                      15P 0 OPTIONS(*OMIT:*NOPASS)
003600120319     d
003700120322     D  klin           s              2
003800120322     D  cont           s              3  0
003900120321     D  Leng           s              3  0
004000120321     D  messag         s            250
004100120321     D  messag_uni     s            250
004200921021     C*---------------------------------------------------------------*
004300921021     C     *ENTRY        PLIST
004400970707     C                   PARM                    KPJBA
004500970707     C                   PARM                    DSLV13
004600970707     C                   PARM                    DSSI95
004700000721     C                   PARM                    FLGBAC            1
004800060921     C                   PARM                    tisi97ds
004900120322     C                   PARM                    lingua            2
005000940218     C*---------------------------------------------------------------*
005100970707     C* TIPO LANCIO, I13TLA "C" -           CHIUSO CON LR
005200970707     C* TIPO LANCIO, I13TLA "L" - ELABORO E CHIUDO CON LR
005300970707     C* TIPO LANCIO, I13TLA " " - ELABORO E CHIUDO IN RETRN
005400970707    1C     I13TLA        IFNE      'C'
005500970707     C                   CLEAR                   O13ERR
005600970707     C                   CLEAR                   O13MSG
005700970708     C                   CLEAR                   O13ROP
005800970708     C                   CLEAR                   O13ROC
005900970708     C                   CLEAR                   O13RON
006000970708     C                   CLEAR                   O13ROL
006100970707     C**
006200970701     C                   CALL      'TISI95R'
006300970701     C                   PARM                    DSSI95
006400941019     C*
006500970703     C* NON TROVATI ERRORI
006600970707    2C     O95ERR        IFEQ      *BLANKS
006700120322     c* Se passato 6� parametro, imposto codice lingua
006800120322    3c                   if        parms=6
006900120322     c                   eval      klin =lingua
007000120322     c                   else
007100120322     c                   clear                   klin
007200120322     c                   endif
007300120319     c
007400080407     c* Se richiesto controllo cappario utilizzi particolari
007500120322    3c                   if        parms>=5
007600080407     c                   eval      o97uti='S'
007700060921     c                   call      'TISI97R'
007800060921     c                   parm                    dssi95
007900060921     c                   parm                    tisi97ds
008000060921     c* se c'e' errore imposto ed esco
008100060921    4c                   if        o97err<>' '
008200060921     C                   MOVEL     O97MSG        O13MSG
008300060921     c                   eval      o13err='5'
008400060921   x4c                   else
008500120321     c
008600120321     c* Preparo messaggio specifico
008700120321     c                   exsr      PrepaMSG
008800060921    4c                   endif
008900060921    3c                   endif
009000970707     C**
009100970707     C* LIVELLO AFFIDABILITA'0 : SEMPRE ERRORE
009200970707    3C     I13AF0        IFEQ      'S'
009300970708   3AC     O95LIA        IFEQ      '0'
009400970707     C**
009500970703     C* SE IL CAP L'HA TROVATO DO ERRORE DI INCONGRUENZA
009600970708    4C     O95LID        IFLT      '2'
009700970708     C                   MOVEL     ERR(2)        O13MSG
009800970707     C                   MOVEL     '5'           O13ERR
009900970703     C                   ELSE
010000970708     C                   MOVEL     ERR(1)        O13MSG
010100970707     C                   MOVEL     '5'           O13ERR
010200970707    4C                   ENDIF
010300970703     C*
010400970703     C                   GOTO      ENDC01
010500970708   3AC                   ENDIF
010600970708   X3C                   ELSE
010700970708    4C     O95LID        IFEQ      '1'
010800970708     C     O95NUF        ANDNE     'S'
010900970708     C                   MOVEL     ERR(2)        O13MSG
011000970708     C                   MOVEL     '5'           O13ERR
011100970710     C                   GOTO      ENDC01
011200970708    4C                   ENDIF
011300970708    3C                   ENDIF
011400970704     C*
011500970704     C* CAP E PROVINCIA DEVONO ESSERE CONGRUENTI
011600970707    3C     I13CNG        IFEQ      'S'
011700970707     C     I95TCN        ANDEQ     '3'
011800970707     C*
011900970707    4C     O95NCP        IFEQ      'S'
012000970704     C     I95PRV        ANDNE     O95PRV
012100970704     C     I95PRV        ANDNE     O95PRA
012200970708     C                   MOVEL     ERR(3)        O13MSG
012300970707     C                   MOVEL     '5'           O13ERR
012400970707     C                   GOTO      ENDC01
012500970707    4C                   ENDIF
012600970707    3C                   ENDIF
012700970708     C**
012800970708     C* SE LIVELLO DATI 3 O 4 E AFFIDABILITA' 2 O > PROPONGO I DATI
012900970708     C*  CHE SI SONO MODIFICATI CON MSG DI AVVISO
013000970708    1C     O95LID        IFEQ      '3'
013100970708     C     O95LID        OREQ      '4'
013200970708    2C     O95LIA        IFGE      '2'
013300970708     C*
013400970708    3C     O95PRV        IFNE      I95PRV
013500970714    3C     O95PRA        ANDNE     I95PRV
013600970708     C                   MOVEL     '4'           O13ERR
013700000721     C*
013800120322     C     PARMS         IFge      4
013900000721     C     FLGBAC        ANDEQ     'S'
014000000721     C                   MOVEL     ERR(10)       O13MSG
014100000721     C                   ELSE
014200970708     C                   MOVEL     ERR(8)        O13MSG
014300000721     C                   ENDIF
014400970708     C                   MOVEL     'S'           O13ROP
014500970708    3C                   ENDIF
014600970708    3C     O95NAR        IFNE      I95NAR
014700970708     C                   MOVEL     '6'           O13ERR
014800000721     C*
014900120322     C     PARMS         IFge      4
015000000721     C     FLGBAC        ANDEQ     'S'
015100000721     C                   MOVEL     ERR(10)       O13MSG
015200000721     C                   ELSE
015300970708     C                   MOVEL     ERR(8)        O13MSG
015400000721    3C                   ENDIF
015500000721     C*
015600970708     C                   MOVEL     'S'           O13RON
015700970708    3C                   ENDIF
015800970708    3C     O95CAP        IFNE      I95CAP
015900970708     C                   MOVEL     '5'           O13ERR
016000000721     C*
016100120322     C     PARMS         IFge      4
016200000721     C     FLGBAC        ANDEQ     'S'
016300000721     C                   MOVEL     ERR(10)       O13MSG
016400000721     C                   ELSE
016500970708     C                   MOVEL     ERR(8)        O13MSG
016600000721    3C                   ENDIF
016700000721     C*
016800970708     C                   MOVEL     'S'           O13ROC
016900970708    3C                   ENDIF
017000970708     C     O13ERR        IFNE      ' '
017100970708     C                   GOTO      ENDC01
017200970708    2C                   ENDIF
017300970708    2C                   ENDIF
017400970708    1C                   ENDIF
017500970710     C*
017600000713     C* VEDO SE DARE ERRORE PER LIVELLO DI AFFIDABILITA' = 2 O 1
017700000713     C*  SE NON FACCIO I CONTROLLI SPECIFICI PER LIVELLO 1
017800970710     C     O95LIA        IFEQ      '2'
017900000713     C     I13AF1        OREQ      ' '
018000000713     C     O95LIA        ANDEQ     '1'
018100000713     C*
018200000713     C     O95NUF        IFNE      'S'
018300970710     C     I13LA3        ANDEQ     'S'
018400970710     C     E13FZ3        ANDNE     1
018500970710     C                   MOVEL     '5'           O13ERR
018600970710     C     I13SZ3        IFEQ      'S'
018700970710     C                   MOVEL     ERR(9)        O13MSG
018800970710     C                   MOVEL     '1'           E13FZ3
018900970710     C                   ELSE
019000970710     C                   MOVEL     ERR(1)        O13MSG
019100970710     C                   ENDIF
019200970710     C                   GOTO      ENDC01
019300970710     C                   ENDIF
019400000713     C                   ENDIF
019500970707     C*
019600970707     C* CAP CON VIARIO
019700970707    4C     O95FLG        IFEQ      'S'
019800970707     C*
019900970707    3C     I13CNV        IFEQ      'S'
020000970707     C     E13FZV        ANDNE     1
020100970707     C                   MOVEL     '5'           O13ERR
020200970707     C* ERRORE FORZABILE SE = "S"
020300970707    5C     I13SZV        IFEQ      'S'
020400970708     C                   MOVEL     ERR(5)        O13MSG
020500970707     C                   MOVEL     1             E13FZV
020600970707     C                   ELSE
020700970708     C                   MOVEL     ERR(4)        O13MSG
020800970707    5C                   ENDIF
020900970707     C                   GOTO      ENDC01
021000970707    4C                   ENDIF
021100970707    3C                   ENDIF
021200970707     C**
021300970707     C* PROCEDURA AFFIDABILITA' =1
021400970707     C*  LA 1 VOLTA VADO IN INTERROGAZIONE
021500970707    3C     I13AF1        IFEQ      'S'
021600970707     C     O95LIA        ANDEQ     '1'
021700970707     C*
021800970707    4C     E13FZ1        IFNE      1
021900970707     C* RICHIAMO L'INTERROGAZIONE
022000970707     C                   CLEAR                   DSSI00
022100970707     C                   MOVEL     'L01'         D00OP0
022200970707     C                   MOVEL     I95LOC        D00LOC
022300970707     C                   MOVEL     I95DAT        D00DRI
022400970707     C                   MOVEL     1             E13FZ1
022500970707     C                   CALL      'TISI30R'
022600970707     C                   PARM                    KPJBA
022700970707     C                   PARM                    DSSI00
022800970707     C* SE SELEZIONE
022900970707    5C     D00CAP        IFNE      *BLANKS
023000970707     C                   MOVEL     D00CAP        O95CAP
023100970710     C                   MOVEL     'S'           O13ROC
023200970707     C                   MOVEL     D00LOC        O95LOC
023300970710     C                   MOVEL     'S'           O13ROL
023400970707     C                   MOVEL     D00NAR        O95NAR
023500970710     C                   MOVEL     'S'           O13RON
023600970707     C                   MOVEL     D00PRV        O95PRV
023700970710     C                   MOVEL     'S'           O13ROP
023800970707     C                   MOVEL     '3'           O13ERR
023900970707     C                   GOTO      ENDC01
024000970707    5C                   ENDIF
024100970707    4C                   ENDIF
024200970707     C**
024300970707     C* SE C'E' ANCORA LIVELLO AFFIDABILITA' 1 : ERRORE FORZABILE
024400970707    4C     E13FZ2        IFNE      1
024500970707     C                   MOVEL     '3'           O13ERR
024600970707     C*
024700970707    5C     I13SZ2        IFEQ      'S'
024800970708     C                   MOVEL     ERR(7)        O13MSG
024900970707     C                   MOVEL     1             E13FZ2
025000970707     C                   ELSE
025100970708     C                   MOVEL     ERR(6)        O13MSG
025200970707    5C                   ENDIF
025300970707     C                   GOTO      ENDC01
025400970707    4C                   ENDIF
025500970707     C**
025600970707     C* LA PROVINCIA DEVE PERO' ESSERE CONGRUENTE CON IL CAP ALMENO
025700970707    4C     O95NCP        IFEQ      'S'
025800970707     C     I95PRV        ANDNE     O95PRV
025900970707     C     I95PRV        ANDNE     O95PRA
026000970707     C                   MOVEL     ERR(3)        O13MSG
026100970707     C                   MOVEL     '4'           O13ERR
026200970707     C                   GOTO      ENDC01
026300970707    4C                   ENDIF
026400970707    3C                   ENDIF
026500970707     C**
026600970707   X2C                   ELSE
026700970707     C**
026800970707     C                   MOVEL     O95MSG        O13MSG
026900970707     C* ERRORE SU :
027000970707     C                   SELECT
027100970707     C     O95ERR        WHENEQ    'C'
027200970707     C     O95ERR        OREQ      'E'
027300970707     C                   MOVEL     '4'           O13ERR
027400970707     C     O95ERR        WHENEQ    'D'
027500970707     C     O95ERR        OREQ      'B'
027600970707     C     O95ERR        OREQ      'H'
027700970707     C     O95ERR        OREQ      'I'
027800970707     C                   MOVEL     '3'           O13ERR
027900970707     C     O95ERR        WHENEQ    '1'
028000970715     C                   MOVEL     '6'           O13ERR
028100970707     C                   OTHER
028200970707     C                   MOVEL     '5'           O13ERR
028300970707     C                   ENDSL
028400970707    2C                   ENDIF
028500970707    1C                   ENDIF
028600970703     C*
028700941019     C     ENDC01        TAG
028800941019     C* CHIUSURA PGM
028900970707    1C     I13TLA        IFEQ      ' '
029000941019     C                   RETURN
029100941019     C                   ELSE
029200970701     C*
029300970701     C                   CLEAR                   DSSI95
029400970701     C                   MOVEL     'C'           I95TLA
029500970701     C                   CALL      'TISI95R'
029600970701     C                   PARM                    DSSI95
029700970701     C*
029800941019     C                   SETON                                        LR
029900941019    1C                   ENDIF
030000120321     c*-----------------------------------------------------------------------
030100120321     c     PrepaMSG      BEGSR
030200120321     c
030300120321     c                   movel     o97flo        dcppflo
030400120321     c* se non utilizzabile per il network indicato errore
030500120321    1c                   if        o97uti='N'
030600120321     c
030700120322     c* Nelle tre ccasistiche di errore imposto la prima parte del messaggio
030800120321     c*  che � uguale
030900120321    2c                   if        �cppall='S'  or
031000120321     c                             (�cppcas='S' and i95fca='S') or
031100120321     c                             (�cpppas='S' and i95tpo='A')
031200120321     c                   select
031300120321     c                   when      o97liv='C'
031400120321     c                   clear                   messag
031500120321     c                   eval      messag =
031600120322     c                             rtvMsgLang('TIS0712':kLIN:i95cap+i97ntw)
031700120321     C                   MOVEL     messag        O13MSG
031800120321     C****               MOVEL     ERR(11)       O13MSG
031900120321     c**                 eval      %subst(o13msg:32:3)=i97ntw
032000120321     c
032100120321     c                   when      o97liv='N'
032200120321     C***                MOVEL     ERR(14)       O13MSG
032300120321     c****               eval      %subst(o13msg:33:3)=i97ntw
032400120321     c                   clear                   messag
032500120321     c                   eval      messag =
032600120322     c                             rtvMsgLang('TIS0713':kLIN:i95nar+i97ntw)
032700120321     C                   MOVEL     messag        O13MSG
032800120321
032900120321     c                   when      o97liv='W'
033000120321     C***                MOVEL     ERR(17)       O13MSG
033100120321     c***                eval      %subst(o13msg:9:3)=i97ntw
033200120321     c                   clear                   messag
033300120321     c                   eval      messag =
033400120322     c                             rtvMsgLang('TIS0714':kLIN:i97ntw)
033500120321     C                   MOVEL     messag        O13MSG
033600120321     c                   endsl
033700120321     C                   MOVEL     '5'           O13ERR
033800120321    2c                   endif
033900120321     c*
034000120321     c* errore su spedizioni con C/Assegno
034100120321     c                   clear                   messag
034200120321     c                   eval      cont=1
034300120321     c
034400120321    2c                   if        �cppcas='S' and i95fca='S'
034500120321     c
034600120321    3c                   dow       messag=*blanks
034700120321     c
034800120321    4c                   select
034900120614     c                   when      cont=1 or (klin<>'it' and klin<>'  ')
035000120614     c                   if        klin='it' or klin='  '
035100120614     c                   eval      messag ='per spedizioni con Contrassegno'
035200120614     c                   endif
035300120614     c                   if        klin='en'
035400120614     c                   eval      messag ='for cash-on-delivery shipments'
035500120614     c                   endif
035600120614     c                   if        klin='de'
035700120614     c                   eval      messag ='f�r Nachnahmesendungen'
035800120614     c                   endif
035900120614     c                   if        klin='fr'
036000120614     c                   eval      messag ='pour exp�ditions en contre remb'
036100120614     c                   endif
036200120614     c
036300120321     c                   when      cont=2
036400120321     c                   eval      messag ='per sped.con C/Assegno'
036500120321    4c                   endsl
036600120321     c
036700120321     c                   exsr      CtrLeng
036800120321    3c                   enddo
036900120321     c
037000120321    2c                   endif
037100120321     c
037200120321    2c                   if        �cpppas='S' and i95tpo='A'
037300120321     c
037400120321    3c                   dow       messag=*blanks
037500120321     c
037600120321    4c                   select
037700120614     c                   when      cont=1 or (klin<>'it' and klin<>'  ')
037800120614     c                   if        klin='it' or klin='  '
037900120321     c                   eval      messag ='per spedizioni in porto Assegnato'
038000120614     c                   endif
038100120614     c                   if        klin='en'
038200120614     c                   eval      messag ='for carriage forward shipments'
038300120614     c                   endif
038400120614     c                   if        klin='de'
038500120614     c                   eval      messag ='f�r Sendungen als Warennachnahme'
038600120614     c                   endif
038700120614     c                   if        klin='fr'
038800120614     c                   eval      messag ='pour exp�ditions en port d�'
038900120614     c                   endif
039000120614     c
039100120321     c                   when      cont=2
039200120321     c                   eval      messag ='per spedizioni in Assegnato'
039300120321    4c                   endsl
039400120321     c
039500120321     c                   exsr      CtrLeng
039600120321    3c                   enddo
039700120321     c
039800120321    2c                   endif
039900120321    1c                   endif
040000120321     c
040100120321     c***
040200120321    2c***                if        �cppcas='S' and i95fca='S'
040300120321     c***                select
040400120321     c***                when      o97liv='C'
040500120321     C***                MOVEL     ERR(12)       O13MSG
040600120321     c***                eval      %subst(o13msg:32:3)=i97ntw
040700120321     c***                when      o97liv='N'
040800120321     C***                MOVEL     ERR(15)       O13MSG
040900120321     c***                eval      %subst(o13msg:33:3)=i97ntw
041000120321     c***                when      o97liv='W'
041100120321     C***                MOVEL     ERR(18)       O13MSG
041200120321     c***                eval      %subst(o13msg:9:3)=i97ntw
041300120321     c***                endsl
041400120321     C***                MOVEL     '5'           O13ERR
041500120321    7c***                endif
041600120321    7c***                if        �cpppas='S' and i95tpo='A'
041700120321     c***                select
041800120321     c***                when      o97liv='C'
041900120321     C***                MOVEL     ERR(13)       O13MSG
042000120321     c***                eval      %subst(o13msg:32:3)=i97ntw
042100120321     c***                when      o97liv='N'
042200120321     C***                MOVEL     ERR(16)       O13MSG
042300120321     c***                eval      %subst(o13msg:33:3)=i97ntw
042400120321     c***                when      o97liv='W'
042500120321     C***                MOVEL     ERR(19)       O13MSG
042600120321     c***                eval      %subst(o13msg:9:3)=i97ntw
042700120321     c***                endsl
042800120321     C***                MOVEL     '5'           O13ERR
042900120321    7c***                endif
043000120321    6c***                endif
043100120321    5c***                endif
043200120321     c                   ENDSR
043300120321     c*-----------------------------------------------------------------------
043400120321     c     Ctrleng       BEGSR
043500120321     c                   clear                   messag_uni
043600120321     c                   eval      messag_uni=%trim(o13msg)+' ' +
043700120321     c                             messag
043800120321     c                   eval      leng=%len(%trim(messag_uni))
043900120321     c
044000120322     c                   if        leng>78 and cont<2
044100120321     c                   clear                   messag
044200120321     c                   eval      cont=cont+1
044300120321     c                   else
044400120321     c                   eval      o13msg=messag_uni
044500120321     c                   endif
044600120321     c
044700120321     c                   ENDSR
044800000721**
044900970710C.A.P. incongruente con provincia e/o localita'                                1
045000970707C.A.P. inesistente                                                             2
045100970707C.A.P. incongruente con la provincia                                           3
045200970707Inserito un CAP generico di citta' che ha il Viario                            4
045300970707Inserito un CAP generico di citta' che ha il Viario: premere ENTER per FORZARE 5
045400970707Localita' INESISTENTE per CAP con eccezioni di localita'                       6
045500970707Localita' INESISTENTE per CAP con eccezioni di localita': ENTER per forzare    7
045600970714ATTENZIONE!! Presenti dati incongruenti: modificati.Enter per continuare
045700970710C.A.P. incongruente con provincia e/o localita': ENTER per forzare             9
045800000721ATTENZIONE!! Presenti dati incongruenti: modificati.                          10
045900120319C.A.P. xxxxxxxxx non servito dal network XXx                                  11
046000120321C.A.P. xxxxxxxxx non servito dal network xxx per spedizioni con C/Assegno     12
046100120319C.A.P. xxxxxxxxx non servito dal network xxx per spedizioni in porto Assegnato
046200120321Nazione "xxx" non servita dal network xxx
046300120321
046400120321
046500120321Network xxx non utilizzabile                                                  17
046600080414Network xxx non utilizzabile per spedizioni con C/Assegno                     18
046700080414Network xxx non utilizzabile per spedizioni in porto assegnato                19
