000100071015     H* ACTGRP=*CALLER
000200071015     H DEBUG DECEDIT('0,') DATEDIT(*DMY/)
000300071015     ffnarbx3l  uf   e           k disk
000400071015     FFnarb01l  IF   E           K DISK    rename(fnarb000:fnarb) prefix(a)
000500071015     FFnlbl02l  IF   E           K DISK
000600071015     FFnblp01l  IF   E           K DISK
000700071015     Ftabel00F  IF   E           K DISK
000800071015     FWFGCS00F  O    E             DISK    USROPN
000900071015     FWFGCS01L  IF   E           K DISK    RENAME(WFGCS000:WFGCS1) USROPN
001000071015     Ffnart01L  IF   E           K DISK
001100071015     Ffnblt01L  IF   E           K DISK
001200071015     FPRTF198   O    F  198        PRINTER OFLIND(*INOF)
001300071015     D FIL£6           S              3  0 DIM(30)
001400071015     D §FLE            S              3  0
001500140717     D spuntato        S              2
001600071015     D TCUDS           DS
001700071015     D  F1                     1      1
001800071015     D  F3                     3      3
001900071015     D  F2                     2      2
002000071015     D  F4                     4      4
002100071015     D  F56                    5      6
002200071015     D WLBDA8          DS
002300071015     D  G08DAT                 1      8  0
002400071015     D  G08INV                 9     16  0
002500071015     D  G08ERR                17     17
002600071015     D  G08TGI                18     22  0
002700071015      *
002800071015     d dar5gen       e ds
002900071015     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
003000071015     D  TCU                  398    697
003100071015     D                                     DIM(50)
003200071015     D  KCU                  698    847P 0
003300071015     D                                     DIM(50)
003400071015     D                                     PACKEVEN
003500071015     D  DTU                  848    895P 0
003600071015     D                                     DIM(12)
003700071015     D                                     PACKEVEN
003800071015     d dskiere         ds
003900071015     d  sk1                    1    140  0 dim(20)
004000071015     d  sk2                  141    280  0 dim(20)
004100071015     d  sk3                  281    420  0 dim(20)
004200071015     d  sk4                  421    560  0 dim(20)
004300071015     d  sk5                  561    700  0 dim(20)
004400071015     d  skt                    1    700  0 dim(100)
004500071017     d dskieref        ds
004600071017     d  sk1f                   1    140  0 dim(20)
004700071017     d  sk2f                 141    280  0 dim(20)
004800071017     d  sk3f                 281    420  0 dim(20)
004900071017     d  sk4f                 421    560  0 dim(20)
005000071017     d  sk5f                 561    700  0 dim(20)
005100071018     d  sktf                   1    700  0 dim(100)
005200071015
005300071015     D DSCMZ         E DS
005400071015     D CNCR80        E DS
005500071015     D KPJBA         E DS
005600071015     d trul06ds      e ds
005700071015     d  lin                    1     90  0 dim(30)
005800071019     D FNLG54ds      E DS
005900071015      *
006000071015     D                SDS
006100071015     D  §PGM                   1     10
006200071015     D cmd1            S             66    DIM(01) CTDATA PERRCD(1)
006300071015     D cmd2            S             66    DIM(01) CTDATA PERRCD(1)
006400071015     D cmd3            S             66    DIM(01) CTDATA PERRCD(1)
006500071015     D cmd4            S             66    DIM(01) CTDATA PERRCD(1)
006600071015     d comman          s             80
006700071015     d lenght          s             15  5 inz(80)
006800071015      *----------------------------------------------------------------
006900071015     C                   DO        30            I                 2 0
007000071015     c                   if        fil£6(i) = 0
007100071015     c                   leave
007200071015     c                   endif
007300071015     C                   Z-ADD     fil£6(i)      FLE               3 0
007400071026      * lettura file spia
007500071015      *
007600071015     c     karbx         setll     fnarbx3l
007700071015     c                   do        *hival
007800071015     c     karbx         reade     fnarbx3l
007900071015     c                   if        %eof(fnarbx3l)
008000071015     c                   leave
008100071015     c                   end
008200071015      *
008300071015      * considero solo le tipologie R=resi e D=dirottamenti
008400071015     c                   if        arbtpl <> 'R' and arbtpl <> 'D'
008500071015     c                   iter
008600071015     c                   end
008700071107      * solo resi
008800071107     c                   if        d54tst = '1' and arbtpl <> 'R'
008900071107     c                   iter
009000071107     c                   endif
009100071107      * solo dirottamenti
009200071107     c                   if        d54tst = '2' and arbtpl <> 'D'
009300071107     c                   iter
009400071107     c                   endif
009500071015      *
009600071015     C* PRELEVO DATI BOLLA
009700071015     C     Karb          chain     fnarb01l                           30
009800071017     C     Karb          chain     fnlbl02l                           31
009900071015     C  n31Kblp          chain     fnblp01l                           31
010000071015      *scrittura wf con bolle da stampare successivamente
010100071015     c                   exsr      scriviwf
010200071015     c
010300071015     C* AGGIORNO DATA DI STAMPA
010400071019     C     d54ris        ifeq      *blank
010500071015     C                   move      amgud8        arbdst
010600071015     C                   EXCEPT    agg5
010700071019     c*                  if        gescomm = 'S'
010800071019     c*                  commit
010900071019     c*                  endif
011000071015
011100071015     C                   END
011200071015     C*
011300071015     c                   enddo
011400071015
011500071015     c                   enddo
011600071015      * eliminazione file di work
011700071015     c                   close     wfgcs00f
011800071015      * rilegge wf e produce il tabulato
011900071015     c                   exsr      stampa
012000071015      *
012100071015     c                   Movel(p)  cmd1(1)       comman
012200071015     c                   eval      lenght = %len(%trim(comman))
012300071015     c                   Call      'QCMDEXC'                            97
012400071015     c                   Parm                    comman
012500071015     c                   Parm                    lenght
012600071015     c                   close     wfgcs01l
012700071015     c                   Movel(p)  cmd3(1)       comman
012800071015     c                   eval      lenght = %len(%trim(comman))
012900071015     c                   Call      'QCMDEXC'                            97
013000071015     c                   Parm                    comman
013100071015     c                   Parm                    lenght
013200071015     c                   seton                                        lr
013300071015      *_________________________________________________________
013400071015     c     stampa        begsr
013500071015      *_________________________________________________________
013600071015      * legge la v.l. del w.f. per la stampa
013700071015     c     *loval        setll     wfgcs01l
013800071015     c                   do        *hival
013900071015     c                   read      wfgcs01l                               98
014000071015     c   98              leave
014100071015      * segnacolli non sequenziali prelevo i numeri da dettaglio colli
014200071015      * bolla mamma
014300140717     c                   clear                   skt
014400071015     c                   if        gcsfns = 'S'
014500071015     C     Kart          setll     Fnart01L
014600071015     c                   z-add     0             ncl               3 0
014700071015     c                   do        *hival
014800071015     C     Kart          reade     Fnart01L
014900071015     c                   if        %eof(fnart01l) or ncl = 100
015000071015     c                   leave
015100071015     c                   endif
015200071015     c                   add       1             ncl
015300071015     c                   z-add     artnsc        skt(ncl)
015400071015     c                   enddo
015500071015     c                   endif
015600071015     c
015700071015      * bolla figlia
015800071018     c                   clear                   sktf
015900140718     c                   clear                   spuntato
016000071015     C     Kblt          setll     Fnblt01L
016100071015     c                   z-add     0             ncl               3 0
016200071015     c                   do        *hival
016300071015     C     Kblt          reade     Fnblt01L
016400071015     c                   if        %eof(fnblt01l) or ncl = 100
016500071015     c                   leave
016600071015     c                   endif
016700140717     c* se presente la data entrata vuol dire
016800140717     c* che è già stato etichettato il pacco con la nuova
016900140717     c* etichetta della figlia
017000140717     c                   if        bltdse <> 0
017100140717     c                   eval      spuntato = 'SI'
017200140717     c                   if        gcsfnsf <>'S'
017300140717     c                   leave
017400140717     c                   end
017500140717     c                   end
017600140717     c*
017700140717     c                   if        gcsfnsf = 'S'
017800071015     c                   add       1             ncl
017900071018     c                   z-add     bltnsc        sktf(ncl)
018000140717     c                   endif
018100071015     c                   enddo
018200071015     c
018300071015      *
018400071015     C     GCsDIS        IFNE      SAVDIS
018500071015     C* PRELEVO DESCRIZIONE TIPO DISPOSIZIONE
018600071015     C                   MOVEL     '2D'          COD
018700071015     C                   MOVEL     *BLANKS       KEY
018800071015     C                   MOVEL     GCsDIS        KEY
018900071015     C     KTAB          CHAIN     TABEL                              50
019000071015     C  N50              MOVEL     TBLUNI        DESCR            20
019100071015     C   50              MOVEL     *ALL'?'       DESCR
019200071015     C                   EXCEPT    TESTA
019300071019     C     d54RIS        IFEQ      'R'
019400071015     C                   EXCEPT    TESTAR
019500071015     C                   END
019600071015     C                   EXCEPT    TESTAT
019700071015     C                   SETON                                        99
019800071015     C                   MOVE      GCsDIS        SAVDIS            1
019900071015     C                   END
020000071015      *
020100071015     C  N99
020200071015     COR OF              DO
020300071015     C                   EXCEPT    TESTA
020400071019     C     d54RIS        IFEQ      'R'
020500071015     C                   EXCEPT    TESTAR
020600071015     C                   END
020700071015     C                   EXCEPT    TESTAT
020800071015     C                   END
020900071015     C  N99              SETON                                        99
021000071015      * prima riga dettaglio bolla mamma
021100071015     C                   EXCEPT    RIGA
021200071015      * stampa dettaglio dei segnacolli se non sequenziali
021300071015     c                   if        skt(1) > 0
021400071015     c                   except    segn1
021500071015     c                   endif
021600071015     c                   if        skt(21) > 0
021700071015     c                   except    segn2
021800071015     c                   endif
021900071015     c                   if        skt(41) > 0
022000071015     c                   except    segn3
022100071015     c                   endif
022200071015     c                   if        skt(61) > 0
022300071015     c                   except    segn4
022400071015     c                   endif
022500071015     c                   if        skt(81) > 0
022600071015     c                   except    segn5
022700071015     c                   endif
022800071015      * seconda riga dettaglio bolla figlia
022900071015      * testa se il campo spedizione è pieno per capire se è stato
023000071015      * agganciato il legame altrimenti stampa anomalia
023100071015     c                   if        gcsnspf > 0
023200071015     C                   EXCEPT    RIGAf
023300071015      * stampa dettaglio dei segnacolli se non sequenziali
023400071018     c                   if        sktf(1) > 0
023500071015     c                   except    segn1f
023600071015     c                   endif
023700071018     c                   if        sktf(21) > 0
023800071015     c                   except    segn2f
023900071015     c                   endif
024000071018     c                   if        sktf(41) > 0
024100071015     c                   except    segn3f
024200071015     c                   endif
024300071018     c                   if        sktf(61) > 0
024400071015     c                   except    segn4f
024500071015     c                   endif
024600071018     c                   if        sktf(81) > 0
024700071015     c                   except    segn5f
024800071015     c                   endif
024900071026      * separazione
025000071026     C                   EXCEPT    separa
025100071015     c                   else
025200071015     C                   EXCEPT    errRIGAf
025300071015     c                   endif
025400071015      *
025500071015     c                   enddo
025600071015     c                   endsr
025700071015      *_________________________________________________________
025800071015     c     scriviwf      begsr
025900071015      *_________________________________________________________
026000071015     c                   clear                   wfgcs000
026100071015     c                   move      arbtpl        GCSdis
026200071015     c                   z-add     aarbaas       GCSaas
026300071015     c                   z-add     aarblnp       GCSlnp
026400071015     c                   z-add     aarbnrs       GCSnrs
026500071015     c                   z-add     aarbnsp       GCSnsp
026600071015     c                   z-add     aarblna       GCSlna
026700071015     c                   move      aarbrsm       GCSrsm
026800071015     c                   move      aarbrsd       GCSrsd
026900071015     c                   z-add     aarbznc       GCSznc
027000071026     c                   z-add     aarbfls       GCSfls
027100071026     c                   z-add     aarbncd       GCSncd
027200071015     c                   z-add     aarbnca       GCSnca
027300071015     c                   move      aarbfns       GCSfns
027400071015     c                   z-add     aarbncl       GCSncl
027500071015     c                   z-add     aarbpkf       GCSpkf
027600071015     c                   z-add     aarbvlf       GCSvlf
027700071015      *figlia se non trova legame o blp non scrive nulla
027800071015     c                   if        not *in31
027900071015     c                   z-add     blpaas        GCSaasf
028000071015     c                   z-add     blplnp        GCSlnpf
028100071015     c                   z-add     blpnrs        GCSnrsf
028200071015     c                   z-add     blpnsp        GCSnspf
028300071015     c                   z-add     blplna        GCSlnaf
028400071015     c                   move      blprsm        GCSrsmf
028500071015     c                   move      blprsd        GCSrsdf
028600071015     c                   z-add     blpznc        GCSzncf
028700071026     c                   z-add     blpfls        GCSflsf
028800071015     c                   z-add     blpncd        GCSncdf
028900071015     c                   z-add     blpnca        GCSncaf
029000071015     c                   move      blpfns        GCSfnsf
029100071015     c                   z-add     blpncl        GCSnclf
029200071015     c                   z-add     blppkf        GCSpkff
029300071015     c                   z-add     blpvlf        GCSvlff
029400071015     c                   endif
029500071015      *
029600071011     c                   write     wfgcs000
029700071011     c                   endsr
029800071026     c
029900071015     C     *INZSR        BEGSR
030000071015     C*---------------------------------------------------------------*
030100071015     C     *ENTRY        PLIST
030200071015     C                   PARM                    KPJBA
030300071019     C                   MOVEL     KPJBU         FNLG54ds
030400071015      *
030500071019     c                   move      d54comm       gescomm           1
030600071015      *
030700071015     C                   Z-ADD     1             CODUT
030800071015     C                   CALL      'X§PARUT'
030900071015     C                   PARM                    UTEDSE
031000071015     C                   MOVEL     REC80         CNCR80
031100071015     C                   Z-ADD     0             SAVLNP            3 0
031200071015      *
031300071015     C* VERIFICO SE SI E' COLLEGATA UN SECONDO LIVELLO
031400071015     C     SimTpP        COMP      '2'                                    02
031500071015     C                   Z-ADD     SIMPOU        §FLE
031600071015      *
031700071015     C                   TIME                    WHHUDT           14 0
031800071015     C                   MOVEL     WHHUDT        ORA               6 0
031900071015     C                   MOVE      WHHUDT        GMAUD8            8 0
032000071015     C* GIRO UDATE
032100071015     C                   MOVE      GMAUD8        G08DAT
032200071015     C                   MOVE      *ZEROS        G08INV
032300071015     C                   MOVEL     *BLANK        G08ERR
032400071015     C                   CALL      'XSRDA8'
032500071015     C                   PARM                    WLBDA8
032600071015     C                   Z-ADD     G08INV        AMGUD8            8 0
032700071015     C*
032800071015      *Trasformo la data dal formato AA/MM/GG al formato GG/MM/AA
032900071019     C     d54RIS        IFEQ      'R'
033000071019     C                   MOVE      d54DAT        G08INV
033100071015     C                   MOVE      *ZEROS        G08DAT
033200071015     C                   MOVEL     3             G08ERR
033300071015     C                   CALL      'XSRDA8'
033400071015     C                   PARM                    WLBDA8
033500071015     C                   Z-ADD     G08DAT        GMADRI            8 0
033600071015     C                   END
033700071015     C*
033800071015      * genero file di work in qtemp
033900071015     c                   Movel(p)  cmd2(1)       comman
034000071015     c                   eval      lenght = %len(%trim(comman))
034100071015     c                   Call      'QCMDEXC'                            97
034200071015     c                   Parm                    comman
034300071015     c                   Parm                    lenght
034400071015     c                   open      wfgcs00f
034500071015      * vista logica
034600071015     c                   Movel(p)  cmd4(1)       comman
034700071015     c                   eval      lenght = %len(%trim(comman))
034800071015     c                   Call      'QCMDEXC'                            97
034900071015     c                   Parm                    comman
035000071015     c                   Parm                    lenght
035100071015     c                   open      wfgcs01l
035200071015     c   97              except    errore
035300071015      *
035400071015     C* DEFINIZIONE CHIAVI
035500071015     C     Karbx         KLIST
035600071015     C                   KFLD                    FLE
035700071015     C                   KFLD                    datak             8 0
035800071019     C                   z-add     d54dat        datak
035900071015     C*
036000071015     C     Karb          KLIST
036100071015     C                   KFLD                    arbAAS
036200071015     C                   KFLD                    arbLNP
036300071015     C                   KFLD                    arbNRS
036400071015     C                   KFLD                    arbNSP
036500071015     C*
036600071015     C     Kart          KLIST
036700071015     C                   KFLD                    gcsAAS
036800071015     C                   KFLD                    gcsLNP
036900071015     C                   KFLD                    gcsNRS
037000071015     C                   KFLD                    gcsNSP
037100071015     C*
037200071015     C     Kblp          KLIST
037300071015     C                   KFLD                    LBLAAN
037400071015     C                   KFLD                    LBLLPN
037500071015     C                   KFLD                    LBLNRN
037600071015     C                   KFLD                    LBLNSN
037700071015     C*
037800071015     C     Kblt          KLIST
037900071015     C                   KFLD                    gcsAASf
038000071015     C                   KFLD                    gcsLNPf
038100071015     C                   KFLD                    gcsNRSf
038200071015     C                   KFLD                    gcsNSPf
038300071015     C*
038400071015     C     KTAB          KLIST
038500071015     C                   KFLD                    CODUT
038600071015     C                   KFLD                    COD               2
038700071015     C                   KFLD                    KEY               8
038800071026     c                   movel     *all'_'       lin198          198
038900071015     C*
039000071015     C***--------------------------------------------------------***
039100071015     C* CARICO EVENTUALI ALTRI FILIALI GESTITE DA QUELLA COLLEGATA *
039200071015     C***--------------------------------------------------------***
039300071015     c                   clear                   trul06ds
039400071015     c                   eval      d06cod = '£6'
039500071015     c                   movel     §fle          d06key
039600071015     c                   movel     trul06ds      kpjbu
039700071015     c                   call      'TRUL06R'
039800071015     c                   parm                    kpjba
039900071015     c                   movel     kpjbu         trul06ds
040000071015     c                   movea     lin           fil£6
040100071015     C                   ENDSR
040200071015     C*****************************************************************
040300071015     Ofnarbx00  E            AGG5
040400071015     O                       arbdst
040500071017     OPRTF198   E            TESTA          1 02
040600071015     O                       RAGUT             +  0
040700071015     O                       SIMFEL              25
040800071017     O                                         + 35 '* ELENCO SPEDIZIONI DI '
040900071017     O                                         +  0 'RESO O DIROTTAMENTO *'
041000071015     O                       §PGM               127
041100071015     O                       GMAUD8             147 '  /  /    '
041200071015     O                                         + 10 'PAG:'
041300071015     O                       PAGE          Z   +  1
041400071015     O          E            TESTA          1
041500071015     O                       KNSIF             +  0
041600071015     O                       KNMUS             +  0
041700071015     O                       ORA                147 '  :  :  '
041800071026     O          E            TESTA          1
041900071015     O                                         +  0 'CON DISPOSIZIONI DI:'
042000071026     O                       DESCR             +  1
042100071026     O          E            TESTA       1
042200071026     O                                         +  0 '* Prima riga bolla O'
042300071026     O                                         +  0 'riginale '
042400071026     O                                         +  0 'Seconda riga nuova '
042500071026     O                                         +  0 'Bolla *'
042600071026     O          E            TESTA          0
042700071026     O                                         +  0 '* Prima riga bolla O'
042800071026     O                                         +  0 'riginale '
042900071026     O                                         +  0 'Seconda riga nuova '
043000071026     O                                         +  0 'Bolla *'
043100071026     O          E            TESTA          0
043200071026     O                                         +  0 '* Prima riga bolla O'
043300071026     O                                         +  0 'riginale '
043400071026     O                                         +  0 'Seconda riga nuova '
043500071026     O                                         +  0 'Bolla *'
043600071015     O          E            TESTAR      1
043700071015     O                                         +  0 '---------  RISTAMPA'
043800071015     O                                         +  1 '---------  ELENCO DEL'
043900071015     O                       GMADRI            +  1 '  /  /    '
044000071015     O          E            TESTAT      2
044100071015     O                                            3 'LNP'
044200071015     O                                            8 'LNA'
044300071015     O                                           14 'ANNO'
044400071015     O                                           18 'SER.'
044500071015     O                                           27 'SPEDIZ.'
044600071019     O                                           38 'Mittente'
044700071019     O                                           80 'Destinatario'
044800071026     O                                          123 'F.Sgn'
044900071017     O                                          143 'Segnacol.Dal-Al'
045000071017     O                                          155 'Colli'
045100071017     O                                          166 'Peso Kg'
045200071017     O                                          173 'Zona'
045300140717     O                                          173 'Zona'
045400140717     O                                          182 'Spuntato'
045500071015     O          E            RIGA        2
045600071015     O                       GCsLNP               3
045700071015     O                       GCsLNA               8
045800071015     O                       GCsAAS              14
045900071015     O                       GCsNRS              18
046000071015     O                       GCsNSP        Z     27
046100071019     O                       gcsRSM              65
046200071015     O                       gcsRSD             103
046300071026     O                       gcsfls             122
046400071017     O                       gcsNCD        Z    133
046500071017     O                                          136 '/'
046600071019     O                       gcsNCA        Z    143
046700071019     O                       gcsNCL        Z    155
046800071017     O                       gcspkf        2    166
046900071106     O                       gcsZNC             172
047000071015     O          E            segn1       1
047100071015     O                       sk1(1)        z   + 30
047200071015     O                       sk1(2)        z   +  1
047300071015     O                       sk1(3)        z   +  1
047400071015     O                       sk1(4)        z   +  1
047500071015     O                       sk1(5)        z   +  1
047600071015     O                       sk1(6)        z   +  1
047700071015     O                       sk1(7)        z   +  1
047800071015     O                       sk1(8)        z   +  1
047900071015     O                       sk1(9)        z   +  1
048000071015     O                       sk1(10)       z   +  1
048100071015     O                       sk1(11)       z   +  1
048200071015     O                       sk1(12)       z   +  1
048300071015     O                       sk1(13)       z   +  1
048400071015     O                       sk1(14)       z   +  1
048500071015     O                       sk1(15)       z   +  1
048600071015     O                       sk1(16)       z   +  1
048700071015     O                       sk1(17)       z   +  1
048800071015     O                       sk1(18)       z   +  1
048900071015     O                       sk1(19)       z   +  1
049000071015     O                       sk1(20)       z   +  1
049100071015     O          E            segn2       1
049200071015     O                       sk2(1)        z   + 30
049300071015     O                       sk2(2)        z   +  1
049400071015     O                       sk2(3)        z   +  1
049500071015     O                       sk2(4)        z   +  1
049600071015     O                       sk2(5)        z   +  1
049700071015     O                       sk2(6)        z   +  1
049800071015     O                       sk2(7)        z   +  1
049900071015     O                       sk2(8)        z   +  1
050000071015     O                       sk2(9)        z   +  1
050100071015     O                       sk2(10)       z   +  1
050200071015     O                       sk2(11)       z   +  1
050300071015     O                       sk2(12)       z   +  1
050400071015     O                       sk2(13)       z   +  1
050500071015     O                       sk2(14)       z   +  1
050600071015     O                       sk2(15)       z   +  1
050700071015     O                       sk2(16)       z   +  1
050800071015     O                       sk2(17)       z   +  1
050900071015     O                       sk2(18)       z   +  1
051000071015     O                       sk2(19)       z   +  1
051100071015     O                       sk2(20)       z   +  1
051200071015     O          E            segn3       1
051300071015     O                       sk3(1)        z   + 30
051400071015     O                       sk3(2)        z   +  1
051500071015     O                       sk3(3)        z   +  1
051600071015     O                       sk3(4)        z   +  1
051700071015     O                       sk3(5)        z   +  1
051800071015     O                       sk3(6)        z   +  1
051900071015     O                       sk3(7)        z   +  1
052000071015     O                       sk3(8)        z   +  1
052100071015     O                       sk3(9)        z   +  1
052200070629     O                       sk3(10)       z   +  1
052300071015     O                       sk3(11)       z   +  1
052400071015     O                       sk3(12)       z   +  1
052500071015     O                       sk3(13)       z   +  1
052600071015     O                       sk3(14)       z   +  1
052700071015     O                       sk3(15)       z   +  1
052800071015     O                       sk3(16)       z   +  1
052900071015     O                       sk3(17)       z   +  1
053000071015     O                       sk3(18)       z   +  1
053100071015     O                       sk3(19)       z   +  1
053200071015     O                       sk3(20)       z   +  1
053300070629     O          E            segn4       1
053400071015     O                       sk4(1)        z   + 30
053500071015     O                       sk4(2)        z   +  1
053600071015     O                       sk4(3)        z   +  1
053700071015     O                       sk4(4)        z   +  1
053800071015     O                       sk4(5)        z   +  1
053900070629     O                       sk4(6)        z   +  1
054000071015     O                       sk4(7)        z   +  1
054100071015     O                       sk4(8)        z   +  1
054200071015     O                       sk4(9)        z   +  1
054300071015     O                       sk4(10)       z   +  1
054400071015     O                       sk4(11)       z   +  1
054500071015     O                       sk4(12)       z   +  1
054600071015     O                       sk4(13)       z   +  1
054700071015     O                       sk4(14)       z   +  1
054800071015     O                       sk4(15)       z   +  1
054900071015     O                       sk4(16)       z   +  1
055000071015     O                       sk4(17)       z   +  1
055100071015     O                       sk4(18)       z   +  1
055200071015     O                       sk4(19)       z   +  1
055300071015     O                       sk4(20)       z   +  1
055400071015     O          E            segn5       1
055500071015     O                       sk5(1)        z   + 30
055600071015     O                       sk5(2)        z   +  1
055700071015     O                       sk5(3)        z   +  1
055800071015     O                       sk5(4)        z   +  1
055900071015     O                       sk5(5)        z   +  1
056000071015     O                       sk5(6)        z   +  1
056100071015     O                       sk5(7)        z   +  1
056200071015     O                       sk5(8)        z   +  1
056300071015     O                       sk5(9)        z   +  1
056400071015     O                       sk5(10)       z   +  1
056500071015     O                       sk5(11)       z   +  1
056600071015     O                       sk5(12)       z   +  1
056700071015     O                       sk5(13)       z   +  1
056800071015     O                       sk5(14)       z   +  1
056900071015     O                       sk5(15)       z   +  1
057000071015     O                       sk5(16)       z   +  1
057100071015     O                       sk5(17)       z   +  1
057200071015     O                       sk5(18)       z   +  1
057300071015     O                       sk5(19)       z   +  1
057400071015     O                       sk5(20)       z   +  1
057500071015     O          E            RIGAf       1
057600071015     O                       GCsLNPf              3
057700071015     O                       GCsLNAf              8
057800071015     O                       GCsAASf             14
057900071015     O                       GCsNRSf             18
058000071015     O                       GCsNSPf       Z     27
058100071015     O                       gcsRSMf             65
058200071015     O                       gcsRSDf            103
058300071026     O                       gcsflsf            122
058400071017     O                       gcsNCDf       Z    133
058500071017     O                                          136 '/'
058600071019     O                       gcsNCAf       Z    143
058700071019     O                       gcsNCLf       Z    155
058800071017     O                       gcspkff       2    166
058900071106     O                       gcsZNCf            172
059000140717     O                       spuntato           179
059100071017     O          E            segn1f      1
059200071015     O                       sk1f(1)       z   + 30
059300071015     O                       sk1f(2)       z   +  1
059400071015     O                       sk1f(3)       z   +  1
059500071015     O                       sk1f(4)       z   +  1
059600071015     O                       sk1f(5)       z   +  1
059700071015     O                       sk1f(6)       z   +  1
059800071015     O                       sk1f(7)       z   +  1
059900071015     O                       sk1f(8)       z   +  1
060000071015     O                       sk1f(9)       z   +  1
060100071015     O                       sk1f(10)      z   +  1
060200071015     O                       sk1f(11)      z   +  1
060300071015     O                       sk1f(12)      z   +  1
060400071015     O                       sk1f(13)      z   +  1
060500071015     O                       sk1f(14)      z   +  1
060600071015     O                       sk1f(15)      z   +  1
060700071015     O                       sk1f(16)      z   +  1
060800071015     O                       sk1f(17)      z   +  1
060900071015     O                       sk1f(18)      z   +  1
061000071015     O                       sk1f(19)      z   +  1
061100071015     O                       sk1f(20)      z   +  1
061200071017     O          E            segn2f      1
061300071015     O                       sk2f(1)       z   + 30
061400071015     O                       sk2f(2)       z   +  1
061500071015     O                       sk2f(3)       z   +  1
061600071015     O                       sk2f(4)       z   +  1
061700071015     O                       sk2f(5)       z   +  1
061800071015     O                       sk2f(6)       z   +  1
061900071015     O                       sk2f(7)       z   +  1
062000071015     O                       sk2f(8)       z   +  1
062100071015     O                       sk2f(9)       z   +  1
062200071015     O                       sk2f(10)      z   +  1
062300071015     O                       sk2f(11)      z   +  1
062400071015     O                       sk2f(12)      z   +  1
062500071015     O                       sk2f(13)      z   +  1
062600071015     O                       sk2f(14)      z   +  1
062700071015     O                       sk2f(15)      z   +  1
062800071015     O                       sk2f(16)      z   +  1
062900071015     O                       sk2f(17)      z   +  1
063000071015     O                       sk2f(18)      z   +  1
063100071015     O                       sk2f(19)      z   +  1
063200071015     O                       sk2f(20)      z   +  1
063300071017     O          E            segn3f      1
063400071015     O                       sk3f(1)       z   + 30
063500071015     O                       sk3f(2)       z   +  1
063600071015     O                       sk3f(3)       z   +  1
063700071015     O                       sk3f(4)       z   +  1
063800071015     O                       sk3f(5)       z   +  1
063900071015     O                       sk3f(6)       z   +  1
064000071015     O                       sk3f(7)       z   +  1
064100071015     O                       sk3f(8)       z   +  1
064200071015     O                       sk3f(9)       z   +  1
064300071015     O                       sk3f(10)      z   +  1
064400071015     O                       sk3f(11)      z   +  1
064500071015     O                       sk3f(12)      z   +  1
064600071015     O                       sk3f(13)      z   +  1
064700071015     O                       sk3f(14)      z   +  1
064800071015     O                       sk3f(15)      z   +  1
064900071015     O                       sk3f(16)      z   +  1
065000071015     O                       sk3f(17)      z   +  1
065100071015     O                       sk3f(18)      z   +  1
065200071015     O                       sk3f(19)      z   +  1
065300071015     O                       sk3f(20)      z   +  1
065400071017     O          E            segn4f      1
065500071015     O                       sk4f(1)       z   + 30
065600071015     O                       sk4f(2)       z   +  1
065700071015     O                       sk4f(3)       z   +  1
065800071015     O                       sk4f(4)       z   +  1
065900071015     O                       sk4f(5)       z   +  1
066000071015     O                       sk4f(6)       z   +  1
066100071015     O                       sk4f(7)       z   +  1
066200071015     O                       sk4f(8)       z   +  1
066300071015     O                       sk4f(9)       z   +  1
066400071015     O                       sk4f(10)      z   +  1
066500071015     O                       sk4f(11)      z   +  1
066600071015     O                       sk4f(12)      z   +  1
066700071015     O                       sk4f(13)      z   +  1
066800071015     O                       sk4f(14)      z   +  1
066900071015     O                       sk4f(15)      z   +  1
067000071015     O                       sk4f(16)      z   +  1
067100071015     O                       sk4f(17)      z   +  1
067200071015     O                       sk4f(18)      z   +  1
067300071015     O                       sk4f(19)      z   +  1
067400071015     O                       sk4f(20)      z   +  1
067500071017     O          E            segn5f      1
067600071015     O                       sk5f(1)       z   + 30
067700071015     O                       sk5f(2)       z   +  1
067800071015     O                       sk5f(3)       z   +  1
067900071015     O                       sk5f(4)       z   +  1
068000071015     O                       sk5f(5)       z   +  1
068100071015     O                       sk5f(6)       z   +  1
068200071015     O                       sk5f(7)       z   +  1
068300071015     O                       sk5f(8)       z   +  1
068400071015     O                       sk5f(9)       z   +  1
068500071015     O                       sk5f(10)      z   +  1
068600071015     O                       sk5f(11)      z   +  1
068700071015     O                       sk5f(12)      z   +  1
068800071015     O                       sk5f(13)      z   +  1
068900071015     O                       sk5f(14)      z   +  1
069000071015     O                       sk5f(15)      z   +  1
069100071015     O                       sk5f(16)      z   +  1
069200071015     O                       sk5f(17)      z   +  1
069300071015     O                       sk5f(18)      z   +  1
069400071015     O                       sk5f(19)      z   +  1
069500071015     O                       sk5f(20)      z   +  1
069600071026     O          E            separa      1
069700071026     o                       lin198             198
069800071015     O          E            errRIGAf    1
069900071015     O                                         + 25 '* Attenzione non trovata '
070000071015     O                                         +  0 'spedizione figlia verifi'
070100071015     O                                         +  0 'care i dati della bolla *'
070200071015     O          E            errore         1 02
070300071015     O                       RAGUT             +  0
070400071015     O                       SIMFEL              25
070500071015     O                                         + 25 '* ELENCO SPEDIZIONI DA '
070600071015     O                                         +  0 'PORTARE NEL MAGAZZINO '
070700071015     O                                         +  0 'GIACENZE *'
070800071015     O                       §PGM               127
070900071015     O                       GMAUD8             147 '  /  /    '
071000071015     O                                         + 10 'PAG:'
071100071015     O                       PAGE          Z   +  1
071200071015     O          E            errore      2
071300071015     O                                         + 25 '* Attenzione errore in '
071400071015     O                                         +  0 'esecuzione programma  '
071500071015     O                                         +  0 'avvisare il CED *'
071600071015      *_________________________________________________________________________
071700071015**  CMD1
071800071015DLTF FILE(QTEMP/WFGCS00F)
071900071015**  CMD2
072000071015CRTDUPOBJ OBJ(WFGCS00F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
072100071015**  CMD3
072200071015DLTF FILE(QTEMP/WFGCS01L)
072300071015**  CMD4
072400071011CRTDUPOBJ OBJ(WFGCS01L) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
