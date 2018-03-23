000100020326     H DECEDIT('0,') DATEDIT(*DMY.)
000101020717     h* essendo un pgm sql è consigliabile compilarlo con l'opzione PC
000102020717     h* aggiungendo prima nella lista di librerie la PAXI1O3
000200020409     H**********************************************************************
000300020409     H* Il pgm può essere lanciato impostando la filiale oppure se sono un
000400020409     H* primo livello posso lanciare tutte le mie seconde.
000500020409     H* La data fino al.. è obbligatoria (§41ddc) se si lancia la stampa del
000600020530     H* mandato mentre se si lancia la ristampa no(§41ddc=0). E' questa data
000700020409     H* che pilota l'istruzione sql da eseguire
000800020409     H*
000900020409     H* STAMPA MANDATO FINO AL... (tutti quelli non stampati fino al...)
001000020409     H*
001100021111     H*SELECT substr(digits(fttpdr), 1, 3) as pippo, fttddc, fttpdr, fttdiv, ftttim,
001200020409     H*ifnull(FRepdr, 0), ifnull(frepno, 0), ifnull(fretim, 0),ifnull(frenot
001300020612     H*, ' '), ifnull(frecre, ' '), (fttitt+fttita+ftttpe+ftttbn+fttmnt),
001400060627     h*ifnull(fredds, 0), fttflr FROM fiftt00f left outer
001500020409     H*join fifre00f on fttpdr = frepdr and fttddc = freddc where fttfvl =
001600020614     H*'C' and ftttsr = ' ' and fttddc <= 20020101 and substr(fttflr, 1 ,1 =
001700020614     H* ' ') and substr(digits(fttpdr), 1, 3) = '001'
001800020409     H*ORDER BY pippo, fttddc, fttpdr, frecre, frepno
001900020530     h*
002000020530     H*                        Spiegazione istruzione
002100020530     h*
002200020409     H*Unisce i file fiftt00f e fifre00f in join, estraggo record che hanno
002300020530     H*FTTFVL='C', FTTTSR =*blanks, FTTDDC<=§41ddC, FREDDS=*blanks oppure
002400020530     H*FTTFLR=*blanks(Flag x stampa mandato), Filiale=filiale richiesta.
002500020530     H*Viene ordinato per fil.,data conteggio padroncino, causale rettifica
002600020409     H*
002700020612     H* RISTAMPA MANDATO DAL...AL...(tutti le variazioni dal...al...
002701020612     h* sia che siano già stampate oppure no. Per quelle non ancora
002702020612     h* stampare metto una segnalazione in stampa)
002800020409     H*
002900021111     H*SELECT substr(digits(fttpdr), 1, 3) as pippo, fttddc, fttpdr, fttdiv, ftttim
003000020409     H*ifnull(frepdr, 0), ifnull(frepno, 0), ifnull(fretim, 0),
003100020612     H*ifnull(frenot, ' '), ifnull(frecre, ' '),(fttitt+fttita+ftttpe+ftttbn
003201020612     h*+fttmnt), ifnull(fredds, 0), fttflr  FROM fiftt00f left outer
003300020409     H*join fifre00f on fttpdr = frepdr and fttddc = freddc WHERE fttfvl =
003400020409     H*'C' and ftttsr = ' ' and fttddc >= 20020101 and fttddc <= 20020101
003500020612     H*and substr(digits(fttpdr), 1, 3) = '001'
003700020409     H* order by pippo, fttddc, fttpdr
003800020530     h*
003900020530     H*                        Spiegazione istruzione
004000020530     h*
004100020409     H*Unisce i file fiftt00f e fifre00f in join, estrggo i record che hanno
004200020530     H*FTTFVL='C', FTTTSR =*blanks,FTTDDC compresa tra limiti date richieste,
004400020612     H*Filiale=filiale richiesta
004500020409     H*ordinando per filiale, data conteggio padroncino
004600020409     H*
004700020409     H*In questa stampa è possibile scegliere se farla dettagliata oppure no
004800020409     H*quindi nell'istruzione sql ci sarà in più il controllo sul campo
004900020612     H*FRETIM<>0                                                            -
005113020613     h*
005200020409     H**********************************************************************
005300020327     Fazorg01l  IF   E           K DISK
005400030123     Ffifre02l  uF   E           K DISK    commit
005500020327     Ffiftt02l  uF   E           K DISK    commit
005600021203     FFIAPD01l  IF   E           K DISK
005700020327     Ftntbe01l  IF   E           K DISK
005800020327     Fficn42P1  O    E             PRINTER OFLIND(*in30)
005900020327     D KPJBA         E DS
006000020606     D dftt01        E DS
006300020327     D DSUL06        E DS                  EXTNAME(TRUL06DS)
006400020327     D  LIN                    1     90  0
006500020327     D                                     DIM(30)                              FIL. COMODO
006600020327     D ut§dse        E DS                  extname(ut§dse0f)
006700020327     D CNCR80        E DS
006800020327     D ficn41ds      E DS
007000020326     D WrkSqlCmd       S           1024
007100020327     D salpdr          S                   like(frepdr) inz
007200020327     D salddc          S                   like(freddc) inz
007300020327     D salfil          S              3
007400020327     d com3            s              3
007500020327     d comdta          s              8
007600020327     d comdtaa         s              8
007700020326     d dtaiso          s               d
007800020326     d dtaeur          s               d   datfmt(*eur)
007900020327     D conta           S                   like(§41fil) inz
007901021111     D errpro          S              1  0 inz
007902021112     D errpo           S              1  0 inz
008000020327     c*
008100020530     D fifre           DS
008200020326     d fils                           3
008300020327     d ddcs                                like(fttddc)
008400020327     d pdrs                                like(fttpdr)
008500020327     d divs                                like(fttdiv)
008600021111     d tims                                like(ftttim)
008601021111     d totftt                              like(fretim)
008700020326     d pdrfre                              like(frepdr)
008800020326     d pnofre                              like(frepno)
008900020326     d timfre                              like(fretim)
009000020326     d notfre                              like(frenot)
009100020326     d crefre                              like(frecre)
009102020613     d*ATTENZIONE a questi ultimi due campi
009103020612     d ddsfre                              like(fredds)
009104020612     d flrs                                like(fttflr)
009200020327     c                   exsr      defcam
009300020326     c                   do        y             x
009400020409     c* imposto la filiale nell'istruzione sql
009500020329     c                   move      LIN(x)        com3
009600020326     c                   if        §41ddc <> 0
009700060627     c                   eval      %subst(wrksqlcmd: 480: 3) = com3
009800020326     c                   else
009900060627     c                   eval      %subst(wrksqlcmd: 468: 3) = com3
010000020326     c                   end
010100020327     c                   clear                   fifre
010200020409     c*
010300020326     c* rottura filiale
010400020326     c* Lettura fifTT00F e fifre00f
010500011210     C/EXEC SQL
010600011210     C+ PREPARE S1 FROM :WrkSqlCmd
010700011210     C/END-EXEC
010800011210
010900011210     C/EXEC SQL
011000011210     C+ DECLARE A1 CURSOR FOR S1
011100011210     C/END-EXEC
011200011210
011300011210     C/EXEC SQL
011400011210     C+ OPEN A1
011500011210     C/END-EXEC
011600011210
011700011210     C                   DOU       SqlCod <> 0
011800011205     C/EXEC SQL
011900020326     C+ FETCH NEXT FROM A1 INTO :fifre
012000011205     C/END-EXEC
012100011205     c*
012200011210     C                   SELECT
012300011210     C                   WHEN      SqlCod = 0
012400020613     c* ROTTURA filiale
012500020327     c                   if        fils <> salfil
012600020327     c                   exsr      srrotfil
012700020327     c                   end
012800020613     c* ROTTURA se cambia data
012900020327     c                   if        ddcs <> salddc
013000020326     c                   exsr      srrotdta
013100020326     c                   end
013200020613     c* ROTTURA se cambia padroncino
013300020327     c                   if        pdrs <> salpdr
013400020326     c                   exsr      srrotpdr
013500020326     c                   end
013600020327     c                   movel     divs          div
013601020613     c*
013602020613     c* DETTAGLIO
013603020613     c*
013800020326     c                   if        §41det = 'S'
013801020613     c* per la ristampa segnalo le rettifiche non ancora stampate
013802020613     c                   if        §41ddc = 0 and *in07 and
013803020613     c                             (pnofre= 0 or pnofre=1)
013804020613     c                   movel     flrs          dftt01
013805020613     c                   if        §fttsta = ' ' and ddsfre = 0
013806020613     c                   seton                                        09
013807020613     c                   end
013808020613     c                   end
013809020613     c* totale prestazioni concordate
013900020530     c                   if        *in07
014000020530     c                   z-add     totftt        impcor
014100020530     c                   add       totftt        corgg
014200020530     c                   add       totftt        corfil
014300020530     c                   end
014301020613     c* NO RETTIFICHE
014400020326     c                   if        pdrfre = 0
014401021111     c                   if        tims = 0
014500020326     c                   seton                                        06
014600020326     c                   add       1             conta
014700020326     c                   exsr      lista
014701021111     c                   else
014702021111     c* errore procedura: già cancellate retifiche
014703021111     c                   move      1             errpro
014704021112     c                   move      1             errpo
014705021111     c                   end
014800020326     c                   else
014801020613     c* SI RETTIFICHE
014900020327     c                   add       1             conta
015000020326     c                   exsr      stampa
015100020326     c                   exsr      lista
015200020326     c                   end
015300020530     c                   z-add     0             impcor
015400020327     c* aggiorna il record di ftt
015500020327     c                   if        §41ddc <> 0
015600020327     c     kftt          chain     fiftt02l
015700020327     c                   if        %found(fiftt02l)
015800020606     c                   movel     fttflr        dftt01
015900020606     c                   movel     'S'           §fttsta
016000020606     c                   movel(p)  dftt01        fttflr
016100020327     c                   except    aggio1
016200020327     c                   end
016300020327     c                   end
016400020326     c                   end
016500020326     c* totalizzo
016600020326     c                   if        timfre <> 0
016700020327     c                   add       timfre        imppdr
016800020327     c                   add       timfre        impgg
016900020327     c                   add       timfre        impfil
017000020327     c                   add       timfre        impgen
017100020326     c                   end
017200011210     C                   WHEN      SqlCod = 100
017300011210     C                   WHEN      SqlCod <> 0
017400011210     C                   ENDSL
017500011205     C                   ENDDO
017600011210
017700011205     C/EXEC SQL
017800011205     C+ CLOSE A1
017900011205     C/END-EXEC
018000020326     C                   ENDDO
018100011205     c*
018200020326     C* gestisco gli ultimi totali
018300020326     c                   move      '1'           fine              1
018400020326     c                   exsr      srrotfil
018500020327     c   31              write     firma
018600020326     c                   commit
018700020326     C                   SETON                                        LR
018800020326     C**************************************************************************
018900020326     C* Imposto i campi in stampa e totalizzo
019000020326     C**************************************************************************
019100020326     C     STAMPA        BEGSR                                                  *
019200020326     c* stampo tutta la riga solo la prima riga di nota
019300020327     c                   if        pnofre = 1
019400020326     c*
019500020326     c                   add       1             num
019600020326     c                   seton                                        01
019700020326     C*decodifico causale
019800020327     c                   movel(p)  crefre        tbeke1
019900020326     c     ktbe          chain     tntbe01l
020000020326     c                   if        %found(tntbe01l)
020100020326     c                   movel     tbeuni        descau
020200020327     c                   movel     crefre        causa
020300020326     c                   else
020400020326     c                   movel     *blanks       descau
020500020326     c                   movel     *blanks       causa
020600020326     c                   end
020700020326     c*
020800020327     c                   z-add     timfre        impo
020900020326     c                   else
021000020326     c* stampa solo la nota
021100020327     c                   if        conta > 60
021200020327     c                   seton                                        01
021300020327     c                   else
021400020326     c                   seton                                        02
021500020326     c                   end
021600020327     c                   end
021700020327     c                   movel     notfre        note
021800020326     c*
021900020326     c                   ENDSR
022000020326     C**************************************************************************
022100020326     C* Rottura filiale
022200020326     C**************************************************************************
022300020326     C     Srrotfil      BEGSR                                                  *
022400020326     c* rotture precedenti
022500020326     c                   exsr      srrotdta
022600020327     c*
022700020327     c                   if        §41det = 'S'
022800020327     C* FIRMA (no primo giro)
022900020327     c                   if        primo = *blanks
023000020326     c                   seton                                        31
023100020326     c                   end
023200020326     c* stampa la riga di totale
023300021112     c                   if        corfil <> 0 or impfil <> 0
023400020326     c                   seton                                        05
023500020326     c                   end
023501021112     c* se durante l'elaborazione ho riscontrato che mancano rettifiche
023502021112     c* non stampo il totale del po e metto la segnalazione
023503021112     c                   if        errpo = 1
023504021112     c                   seton                                        15
023505021112     c                   setoff                                       05
023600020327     c                   end
023601021112     c                   end
023700020327     c*
023800020327     c                   if        conta <> 60 or conta <> 0
023900020327     c                   exsr      lista
024000020327     c                   end
024100020327     c* no ultimo giro
024200020327     c                   if        fine = *blanks
024300020327     c                   z-add     60            conta
024400020326     c* decodifico filiale
024500020329     c     LIN(x)        chain     azorg01l
024600020326     c                   if        %found(azorg01l)
024700020329     c                   move      LIN(x)        o41fil
024800020326     c                   movel     orgdes        desfil
024900020326     c                   else
025000020326     c                   Z-ADD     0             o41fil
025100020326     c                   movel     *blanks       desfil
025200020326     c                   end
025300020326     c* azzera il conteggio per la filiale
025400021112     c                   z-add     0             errpo
025401021112     c                   z-add     0             impfil
025500020530     c                   z-add     0             corfil
025600020329     c                   move      LIN(x)        salfil
025700020326     c                   move      desfil        desfils
025800020326     c                   end
025900020530     c                   if        impgen <> 0 and §41det = 'N'
026000020327     c                   write     totgen
026100020327     c                   z-add     0             impgen
026200020327     c                   end
026300020326     c                   ENDSR
026400020326     C**************************************************************************
026500020326     C* Rottura giornaliera
026600020326     C**************************************************************************
026700020326     C     srrotdta      BEGSR                                                  *
026800020326     c* rotture precedenti
026900020327     c                   if        §41det = 'S'
027000020326     c                   exsr      srrotpdr
027100020327     C* STAMPA FIRMA no primo giro no ultimo e se filiale non cambia
027200020327     c                   if        primo = *blanks
027300020327     c                             AND FINE <> '1' and fils = salfil
027400020327     c                   seton                                        31
027500020327     c                   end
027600020327     c                   end
027700020326     c* stampa la riga di totale
027701021111     c                   if        errpro = 0
027800020530     c                   if        corgg <> 0 or impgg <> 0
027900020326     c                   seton                                        04
028000020327     c                   if        §41DET = 'N'
028100020327     c                   add       1             conta
028200020327     c                   end
028300020327     c                   end
028301021111     c                   else
028302021111     c                   seton                                        15
028303021112     C                   setoff                                       31
028304021111     c                   end
028400020327     c*
028500020327     c                   if        conta <> 60 or conta <> 0
028600020327     c                   exsr      lista
028700020327     c                   end
028800020327     c* salto pagina a rottura data solo per stampa a dettaglio no ultimo giro
028900020327     c                   if        fine = *blanks and
029000020327     c                             §41DET = 'S'
029100020327     c                   z-add     60            conta
029200020327     c                   end
029300020326     c* giro data
029400020327     c                   if        ddcs <> 0
029500020327     C                   move      ddcs          dtaiso
029600020326     C                   move      dtaiso        dtaeur
029700020326     C                   move      dtaeur        eurddc
029800020326     c                   end
029900020326     c* azzera il conteggio per il giorno
030000020326     c                   z-add     0             impgg
030100020530     c                   z-add     0             corgg
030101021111     c                   z-add     0             errpro
030200020327     c                   move      ddcs          salddc
030300020326     c                   move      eurddc        eurddcs
030400020326     c*
030500020326     c                   ENDSR
030600020326     C**************************************************************************
030700020326     C* Rottura padroncino
030800020326     C**************************************************************************
030900020326     C     Srrotpdr      BEGSR                                                  *
031000020326     c                   if        §41det = 'S'
031100020326     c                   seton                                        07
031200020326     c* decodifica padroncino
031300021203     C     KAPDS         CHAIN     FIAPD01L
031400021203     c                   if        %found(FIAPD01l)
031500100223     c                   movel(p)  apdrsf        despdr
031600020327     c                   move      pdrs          pdr
031700020326     c                   else
031800020326     c                   movel     *blanks       despdr
031900020326     c                   z-add     0             pdr
032000020326    3C                   ENDIF
032100020326     c* stampa la riga di totale padroncino solo se ha più righe di causali
032200020326     c                   if        num > 1
032300020326     c                   seton                                        03
032400020326     c                   add       1             conta
032500020326     c                   exsr      lista
032600020326     c                   end
032700020326     c* aggiorno la data di stampa
032800020327     c                   if        salpdr <> 0 AND §41DDC <> 0
032900020326     c                   exsr      aggio
033000020326     c                   end
033001020613     c                   end
033200020326     c* azzera il conteggio per padroncino
033300020327     c                   z-add     0             num               3 0
033400020326     c                   z-add     0             imppdr
033500020327     c                   move      pdrs          salpdr
033600020326     c*
033700020326     c                   endsr
033800020326     C**************************************************************************
033900020326     C* aggiorno data di stampa
034000020326     C**************************************************************************
034100020326     C     aggio         BEGSR                                                  *
034200020326     c*
034300030123     c     kfre          setll     fifre02l
034400020326     c                   do        *hival
034500030123     c     kfre          reade     fifre02l
034600030123     c                   if        %eof(fifre02l)
034700020326     c                   leave
034800020326     c                   end
034900020326     c                   move      *date         dtaiso
035000020326     c                   move      dtaiso        fredds
035100020326     c                   except    aggior
035200020326     c                   enddo
035300020326     c*
035400020326     c                   endsr
035500020326     C**************************************************************************
035600020326     C* Gestione stampa
035700020326     C**************************************************************************
035800020326     C     LISTA         BEGSR                                                  *
035900020326     c                   if        conta>60
036000020327     c                   seton                                        3007
036100020327     c                   z-add     1             conta
036200020326     c                   end
036300020326     c* testata
036400020326     c   30              write     testa
036500020326     c                   setoff                                       30
036600020326     c* totale padroncino
036700020326     c   03              write     totpdr
036800020326     c* totale giornaliero
036900020326     c   04              write     totgg
037000020326     c* totale po
037100020326     c   05              write     totfil
037101021112     c* riga errore procedura se già cancellate rettifiche (solo per stampa)
037102021112     c   15              write     rigpro
037200020326     c* firma
037300020326     c   31              write     firma
037400020326     c                   setoff                                       31
037500020326     c* riga NO RETTIFICHE
037600020326     c   06              write     rignor
037700020326     c* riga ROTTURA PADRONCINO
037800020326     c                   if        *in01 and not *in07
037900020326     c                   write     space
038000020326     c                   end
038100020326     c   01              write     riga
038200020327     c   01              setoff                                       07
038300020326     c* riga NOTA
038400020326     c   02              write     rignot
038401020612     c* riga rettifiche non ancora stampate (solo per ristampa)
038402020612     c   09              write     rigerr
038405020612     c*
038500020327     C                   IF        *IN01 OR *IN06
038600020327     c                   movel     *blanks       primo
038700020327     C                   END
038800020327     c                   movea     '000000'      *in(01)
038801020612     c                   movea     '0'           *in(09)
038802021111     c                   movea     '0'           *in(15)
038900020326     c                   ENDSR
039000020326     C**************************************************************************
039100020326     C* INIZIALIZZAZIONE
039200020326     C**************************************************************************
039300020326     C     DEFCAM        BEGSR                                                  *
039400020326     C     *ENTRY        PLIST                                                  *
039500020326     C                   PARM                    KPJBA                          *
039600020326     c                   movel     kpjbu         ficn41ds
039700020326     C                   Z-ADD     1             CODUT
039800020326     C                   CALL      'X§PARUT'
039900020326     C                   PARM                    UT§DSE
040000020326     C                   MOVEL     RAGUT         RSUT
040100020326     C                   MOVEL     REC80         CNCR80
040200020530     c     §41det        comp      'S'                                    08
040300020326     C*                                                    *
040400020326     C     Ktbe          KLIST
040500020326     C                   KFLD                    tbecod
040600020326     C                   KFLD                    tbeke1
040700020326     c                   movel     'CRE'         tbecod
040701021203     C*                                                    *
040702021203     C     KAPDS         KLIST
040703021203     C                   KFLD                    APDTIP
040704021203     C                   KFLD                    PDRS
040705021203     c                   movel     'A'           APDTIP
040800020326     C*                                                    *
040900020326     C     Kfre          KLIST
040901030123     C                   KFLD                    ktip
041000020327     C                   KFLD                    salpdr
041100020327     C                   KFLD                    salddc
041101030123     C                   move      'A'           ktip              1
041200020327     C     Kftt          KLIST
041300020327     C                   KFLD                    pdrs
041400020327     C                   KFLD                    fvls
041500020327     C                   KFLD                    ddcs
041600020327     C                   KFLD                    tsrs
041700020327     c                   movel     'C'           fvls              1
041800020327     c                   movel     *blanks       tsrs              1
041900020326     c*
042000020326     c                   move      *date         udate8
042100020326     c* mi serve per non stampare la riga della firma al primo giro
042200020326     c                   moveL     '1'           primo             1
042300020326     c* carico la schiera dei po da leggere
042400020329     c                   clear                   LIN
042500020326     c                   if        §41fil <> 0
042600020326     c                   z-add     1             y
042700020329     c                   move      §41fil        LIN(1)
042800020326     c                   else
042900020326     c                   exsr      srfil
043000020326     c                   end
043001030317     c* compongo istruzione sql
043002030317     c                   exsr      istruz
043100020326     c* stampa mandato se ddc <> 0
047500020326     c*
047600020326     c     §41det        comp      'S'                                    11
047700020326     c*
047800020326     C                   ENDSR
047900020327     C**************************************************************************
048000030317     C* mi compongo l'istruzione sql
048100020327     C**************************************************************************
048200030317     C     istruz        BEGSR                                                  *
048201030317     c                   if        §41ddc <> 0
048203030317     c                   move      §41ddc        dtaiso
048204030317     c                   move      dtaiso        dtaeur
048205030317     c                   move      dtaeur        o41ddc
048206030317     c* MI COMPONGO L'ISTRUZIONE SQL per mandato
048207030317     C                   EVAL      WrkSqlCmd
048208030317     C                             =
048209030317     C                             'SELECT substr(digits(fttpdr), 1, 3) -
048210030317     c                              as pippo, fttddc, fttpdr, fttdiv, -
048211030317     c                              ftttim, -
048212030317     c                              (fttitt+fttita+ftttpe+ftttbn+fttmnt), -
048213030317     c                              ifnull(FRepdr, 0), -
048214030317     c                              ifnull(frepno, 0), -
048215030317     c                              ifnull(fretim, 0), -
048216030317     c                              ifnull(frenot, '' ''), -
048217030317     c                              ifnull(frecre, '' ''), -
048219030317     c                              ifnull(fredds, 0), -
048220060627     c                              fttflr -
048221030317     c                              from fiftt00f left outer join -
048222030317     c                              fifre00f on fttpdr = frepdr -
048224060627     c                              and fttddc = freddc  -
048225060627     c                              and fretip = ''A'' where -
048226030317     c                              fttfvl = ''C'' and ftttsr = '' '' -
048227030317     c                              and fttddc <= '
048228030317     c                             + %editc(§41ddc:'X')
048229030317     c                             + ' and substr(fttflr, 1, 1)='' '' -
048230030317     c                              and substr(digits(fttpdr), 1, 3) -
048231060627     C                              = ''001'' -
048233030317     c                              ORDER BY pippo, fttddc, fttpdr, frecre, -
048234030317     c                              frepno '
048235030317     c                   else
048236030317     c* ristampa
048237030317     c                   seton                                        10
048239030317     c                   move      §41dds        dtaiso
048240030317     c                   move      dtaiso        dtaeur
048241030317     c                   move      dtaeur        o41dds
048242030317     c                   move      §41ads        dtaiso
048243030317     c                   move      dtaiso        dtaeur
048244030317     c                   move      dtaeur        o41ddc
048245030317     c* MI COMPONGO L'ISTRUZIONE SQL x ristampa e dettaglio
048249030317     C                   EVAL      WrkSqlCmd
048250030317     C                             =
048251030317     C                             'SELECT substr(digits(fttpdr), 1, 3) -
048252030317     c                              as pippo, fttddc, fttpdr, fttdiv, -
048253030317     c                              ftttim, -
048254030317     c                              (fttitt+fttita+ftttpe+ftttbn+fttmnt), -
048255030317     c                              ifnull(FRepdr, 0), -
048256030317     c                              ifnull(frepno, 0), -
048257030317     c                              ifnull(fretim, 0), -
048258030317     c                              ifnull(frenot, '' ''), -
048259030317     c                              ifnull(frecre, '' ''), -
048261030317     c                              ifnull(fredds, 0), -
048262030317     c                              fttflr -
048263030317     c                              from fiftt00f left outer join -
048264030317     c                              fifre00f on fttpdr = frepdr -
048265060627     c                              and fttddc = freddc  -
048266060627     c                              and fretip = ''A'' where -
048267030317     c                              fttfvl = ''C'' and ftttsr = '' '' -
048268030317     c                              and fttddc between '
048269030317     c                             + %editc(§41dds:'X')
048271030317     C                             + ' and ' + %editc(§41ads:'X')
048275030317     c                             + ' and substr(digits(fttpdr), 1, 3) -
048276060627     C                              = ''001'''
048278030317     c                   if        §41det = 'N'
048279030317     C                   EVAL      WrkSqlCmd
048280030317     C                             =
048281030317     C                             %TRIMR(WrkSqlCmd)
048284030317     c                             +
048285030317     c                             ' and fretim <> 0 '
048286030317     c                   end
048287030317     C                   EVAL      WrkSqlCmd
048288030317     C                             =
048289030317     C                             %TRIMR(WrkSqlCmd)
048291030317     c                             +
048292030317     c                             ' ORDER BY pippo, fttddc, fttpdr '
048328030317     c                   end
048329030317     C                   ENDSR
048330030317     C**************************************************************************
048331030317     C* se sono primo livello reperisco i miei secondi livelli
048332030317     C**************************************************************************
048333030317     C     srfil         BEGSR                                                  *
048334020329     C*
048335020717     c                   if        simtpp = '2'
048700020329     C***
048800020329     C* CARICO TABELLA FILIALI GEST.ARRIVI £6
048900020329     C***
049000020329     C                   CLEAR                   DSUL06
049100020329     C                   MOVE      '£6'          D06COD
049200020717     C                   MOVEL     simpou        D06KEY
049300020329     C                   MOVEL     'L'           D06TLA
049400020329     C                   MOVEL     DSUL06        KPJBU
049500020329     C                   CALL      'TRUL06R'
049600020329     C                   PARM                    KPJBA
049700020329     C                   MOVEL     KPJBU         DSUL06
049800020329     C                   ELSE
049900020327     C***
050000020327     C* CARICO TABELLA FILIALI GESTITE £1
050100020327     C***
050200020327     C                   CLEAR                   DSUL06
050300020327     C                   MOVE      '£1'          D06COD
050400020327     C                   MOVEL     SIMFEL        D06KEY
050500020327     C                   MOVEL     ' '           D06TLA
050600020327     C                   MOVEL     DSUL06        KPJBU
050700020327     C                   CALL      'TRUL06R'
050800020327     C                   PARM                    KPJBA
050900020327     C                   MOVEL     KPJBU         DSUL06
051000020329     C                   END
051100020327     c* y= è il numero dei po da gestire
051200020327     c                   z-add     0             y                 3 0
051300020327     c                   do        30            x                 3 0
051400020329     c                   if        LIN(X) <> 0
051500020327     c                   add       1             y
051600020327     c                   end
051700020327     c                   enddo
051800020327     c*
051900020327     C                   ENDSR
052000020326     ofifre000  e            aggior
052100020326     o                       fredds
052200020327     ofiftt000  e            aggio1
052300020327     o                       fttflr
