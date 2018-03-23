000100020212      *---------------------------------------------------------------*
000200031126      * Gestione tabella XT tipo equipaggiamento                      *
000300020212      *---------------------------------------------------------------*
000400020212
000500020212     H DECEDIT('0,') DATEDIT(*DMY.)
000600020212
000700020212      *---------------------------------------------------------------*
000800020212      *   A R C H I V I                                               *
000900020212      *---------------------------------------------------------------*
001000020212      *
001100031126     FTabel00f  UF A E           K DISK
001400020212      *
001500031126     FTrtb04D   CF   E             WORKSTN infds(dsfmt)
001600020212
001700020212      *---------------------------------------------------------------*
001800020212      *   S C H I E R E                                               *
001900020212      *---------------------------------------------------------------*
002000020212      *
002100020215     D Opz             s             15    dim(06) ctdata perrcd(1)
002200020212      *
002300021025     D Msg             s             78    dim(16) ctdata perrcd(1)
002400020212      *
002501031126     D DSFMT           DS
002502031126     D  $TASTO               369    369
002503031126     D  NRG                  370    370
002504031126     D  NCL                  371    371
002505031126     D* posizione cursore
002506031126     D CURSOR          DS
002507031126     D  RIRI                   1      2B 0 INZ
002508031126     D  R$$                    2      2
002509031126     D  COCO                   3      4B 0 INZ
002510031126     D  C$$                    4      4
002600020913      *
002800020212
002900020212      *---------------------------------------------------------------*
003000020212      *   S T R U T T U R E   D A T I                                 *
003100020212      *---------------------------------------------------------------*
003200020212      *
003300020212      * Parametri
003400020212     D KPJBA         e ds
003501031126     D Tabelds       e ds                  extname(tabel00f) inz
004202031126     D Dsxu          e ds                  inz
004203031126     D Dsxt          e ds                  inz
004204031118     D Ptr             S               *
004205031126     D                                     INZ(%ADDR(§xtcxu1))
004206031126     D equi            S                   LIKE(§xtcxu1)
004207031126     D                                     DIM(20)
004208031118     D                                     BASED(Ptr)
004900020724     D TIBS34DS      e ds                  inz
005000020724     D DDATIUTE      e ds                  inz
005100020724     D AZUTEDS       e ds                  inz extname(AZUTE00F)
005101031126     D ccd             s              2    DIM(20) inz
005102031126     D des             s             23    DIM(20) inz
005103031126     D com20           s             20    inz('00000000000000000000')
005104031126     D com2            s              2    inz
006500020212
006600020212      *---------------------------------------------------------------*
006700020212      *   V A R I A B I L I                                           *
006800020212      *---------------------------------------------------------------*
006900020212      *
007000020212     D XX              s              2  0 inz
007001031126     D yy              s              2  0 inz
007800020913     D TipVid          s              1a   inz('0')
007900020212     D Win             s             99a   inz(*zeros)
008000020301     D wTasto          s              2a   inz(*zeros)
008200031126     d equipag         s             40
008201031126     iTB42V2
008202031126     i              V2CCD01                     ccd(1)
008203031126     i              V2CCD02                     ccd(2)
008204031126     i              V2CCD03                     ccd(3)
008205031126     i              V2CCD04                     ccd(4)
008206031126     i              V2CCD05                     ccd(5)
008207031126     i              V2CCD06                     ccd(6)
008208031126     i              V2CCD07                     ccd(7)
008209031126     i              V2CCD08                     ccd(8)
008210031126     i              V2CCD09                     ccd(9)
008211031126     i              V2CCD10                     ccd(10)
008212031126     i              V2CCD11                     ccd(11)
008213031126     i              V2CCD12                     ccd(12)
008214031126     i              V2CCD13                     ccd(13)
008215031126     i              V2CCD14                     ccd(14)
008216031126     i              V2CCD15                     ccd(15)
008217031126     i              V2CCD16                     ccd(16)
008218031126     i              V2CCD17                     ccd(17)
008219031126     i              V2CCD18                     ccd(18)
008220031126     i              V2CCD19                     ccd(19)
008221031126     i              V2CCD20                     ccd(20)
008222031126     i              v2dcd01                     des(1)
008223031126     i              v2dcd02                     des(2)
008224031126     i              v2dcd03                     des(3)
008225031126     i              v2dcd04                     des(4)
008226031126     i              v2dcd05                     des(5)
008227031126     i              v2dcd06                     des(6)
008228031126     i              v2dcd07                     des(7)
008229031126     i              v2dcd08                     des(8)
008230031126     i              v2dcd09                     des(9)
008231031126     i              v2dcd10                     des(10)
008232031126     i              v2dcd11                     des(11)
008233031126     i              v2dcd12                     des(12)
008234031126     i              v2dcd13                     des(13)
008235031126     i              v2dcd14                     des(14)
008236031126     i              v2dcd15                     des(15)
008237031126     i              v2dcd16                     des(16)
008238031126     i              v2dcd17                     des(17)
008239031126     i              v2dcd18                     des(18)
008240031126     i              v2dcd19                     des(19)
008241031126     i              v2dcd20                     des(20)
008400020212
008500020212      *---------------------------------------------------------------*
008600020212      *   M A I N   L I N E                                           *
008700020212      *---------------------------------------------------------------*
008800020212      *  Riepilogo indicatori utilizzati:                             *
008900020212      *  --------------------------------                             *
009000020212      *  01 - Record inesistente (inserimento)                        *
009100020212      *  02 - Record esistente   (modifica)                           *
009300020301      *  20 - Comodo                                                  *
009400020212      *  22 - Errori in scrittura record (WRITE)                      *
009500020212      *---------------------------------------------------------------*
010000020212      * Gestione video
010100031117     C                   do        *hival
010101031117     c                   select
010300031117     C                   when      TipVid='1'
010301031117     c                   exsr      gesv1
010302031117     C                   when      TipVid='2'
010303031117     c                   exsr      gesv2
010306031117     c                   other
010307031117     c                   exsr      fine
010600031118     C                   endsl
010700020212     C                   enddo
010800020212      *
026600020913      *---------------------------------------------------------------*
026700031117      * Fine   - Tasto funzionale F03 -> Fine programma               *
026800020913      *---------------------------------------------------------------*
026900031117     C     Fine          BEGSR
027000020913      *
027100031117     C                   seton                                        lr
027101031117     c                   return
027200020913      *
027300020913     C                   ENDSR
027400020212      *---------------------------------------------------------------*
027500020212      * GESV1  - Gestione videata selezione codice tabella            *
027600020212      *---------------------------------------------------------------*
027700020212     C     GESV1         BEGSR
027900031117      *
027901031118     C                   clear                   V1cau
027902031118     C                   clear                   V1des
027903031126     C                   clear                   V1cau
027904031126     C                   Z-ADD     9             h1riga
027905031126     C                   Z-ADD     29            h1colo
028400020913      * Scrivo la testata
028500020913     C                   clear                   T1opz
028700020913      *
028701031117     c                   do        *hival
028702031118     C                   write     tb42T1
028800020913      * Se esistono errori sulla videata
028900020913      * emetto la write del formato a indicatori spenti per vedere
029000020913      * le eventuali decodifiche
029100020913     C                   if        *in99
029200020913     C                   movea     *in           Win
029400031117     C                   write     tb42V1
029500020913     C                   movea     Win           *in
029600020913     C                   endif
029700020913      *
029800031117     C                   exfmt     tb42V1
029900020913     C                   eval      *in99 = *off
030000020913     C                   clear                   V1MSG
030100020913      *
030300020913      * F03=Fine
030400031117w   1C                   if        *inKC
030500031117     C                   exsr      fine
030501031117     c                   end
031300020212      * Ricerche
031301031126w   1C                   if        *inKd
032000020212     C                   exsr      SEARCH
032100020212e   1C                   endif
032300020301      * Controllo e decodifiche dati immessi a video
032400020301if  1C                   if        not *in99
032500020212     C                   exsr      CTRV1
032600020301e   1C                   endif
032700020212      *
032800020212      * Passaggio alla videata di dettaglio
032900020212if  1C                   if        not *in99
033100020212     C                   eval      TipVid = '2'
033101031117     c                   leave
033200020212e   1C                   endif
033201031117     c                   enddo
033300020212      *
033400031117     C                   ENDSR
034800020212      *---------------------------------------------------------------*
034900020212      * CTRV1  - Controllo e decodifica prima videata                 *
035000020212      *---------------------------------------------------------------*
035100020212     C     CTRV1         BEGSR
035200020212      *
035400020212     C                   clear                   V1msg
035600031117     C                   clear                   V1des
035700031126      * Codice obbligatoria
035800031117if  1C                   if        V1cau = *blanks
035900031117     C                   seton                                        5199
036000031118     C                   eval      V1msg = Msg(01)
036200031117x   1C                   else
036300031126      * Controllo/Decodifica codice
036301031126     c                   eval      tblkut = 1
036302031126     c                   eval      tblcod = 'XT'
036303031126     c                   eval      tblkey = v1cau
036304031126     c     ktab          chain     tabel00f
036305031126     c                   if        %found(tabel00f) and tblflg = ' '
036306031126     c                   movel     tbluni        dsxt
036307031126     c                   else
036308031126     c                   clear                   dsxt
036309031126e   1C                   endif
036310031126     c                   movel     §xtdes        v1des
037601031117e   1C                   endif
037700020212      *
037800031117     C                   ENDSR
038900020212      *---------------------------------------------------------------*
039000020212      * SEARCH - Ricerche                                             *
039100020212      *---------------------------------------------------------------*
039200020212     C     SEARCH        BEGSR
039201031126     C* determino Riga/Colonna del cursore
039202031126     C                   MOVE      nrg           R$$
039203031126     C                   MOVE      ncl           C$$
039204031126     C                   Z-ADD     RIRI          CURR
039205031126     C                   Z-ADD     COCO          CURC
039300020301      *
0394010311261    C                   SELECT
039402031126     C* FMTD1  - ricerca codice tabella
039403031126    >C                   WHEn      h1nmfl = 'V1CAU'
040404031126     C                   CALL      'TRMZ46R'
040405031126     C                   PARM      'R'           tipo              1
040406031126     C                   PARM      *blanks       key               8
040407031126     C                   PARM      *blanks       des1             65
040408031126     C                   PARM      *blanks       des2             65
040409031126     C                   MOVEL     key           V1cau
040414031126    >C                   WHEn      %subst(h1nmfl: 1: 5) = 'V2CCD'
040415031126     c                   eval      com2 = %subst(h1nmfl: 6: 2)
040416031126     c                   move      com2          xx
040417031126     C                   clear                   tblkey
040418031126     C                   CALL      'X§TABER'
040419031126     C                   PARM      1             tblkut
040420031126     C                   PARM      'XU'          tblcod
040421031126     C                   PARM                    tblkey
040422031126     C                   PARM                    §DES             25
040423031126     c                   if        tblkey <> ' '
040424031126     C                   MOVEL     tblkey        ccd(xx)
040425031126     c                   movel     §des          des(xx)
040426031126     c                   else
040427031126     C                   clear                   ccd(xx)
040428031126     c                   clear                   des(xx)
040429031126     c                   end
040430031126     c                   endsl
041901031126     C* imposto pos. del cursore
041902031126     C                   Z-ADD     CURR          H1RIGA
041903031126     C                   Z-ADD     CURC          H1COLO
042000020212      *
042100020212     C                   ENDSR
042300020212      *---------------------------------------------------------------*
042400020212      * GESV2  - Gestione videata dettaglio dati                      *
042500020212      *---------------------------------------------------------------*
042600020212     C     GESV2         BEGSR
042700020212      *
042800020212      * Inizializzazione videata
043000020212     C                   exsr      CarV2
043500020212      *
043501031117     c                   do        *hival
043502031118      * Scrivo la testata
043503031118     C                   write     tb42T1
043600020212      * Se esistono errori sulla videata
043700020212      * emetto la write del formato a indicatori spenti per vedere
043800020212      * le eventuali decodifiche
043900020212     C                   if        *in99
044000020212     C                   movea     *in           Win
044200031117     C                   write     tb42V2
044300020212     C                   movea     Win           *in
044400020212     C                   endif
044500020212      *
044501031118if  1C                   if        *in02
044502031118     C                   write     TB42V2
044503031118     C                   exfmt     PROTECT
044504031118x   1C                   else
045000031117     C                   exfmt     tb42V2
045001031118     c                   end
045200020212     C                   eval      *in99 = *off
045300020301     C                   clear                   V2MSG
045500020212      *
045600020212sel 1C                   select
045700020212      * F03=Fine
045800020212w   1C                   when      *inKC
045900031117     C                   exsr      fine
046100020212      * F12=Ritorno
046200020212w   1C                   when      *inKL
046201031126     c                   Unlock    Tabel00f
046300031117     C                   eval      tipvid = '1'
046400031117     C                   leave
046401031126      * F4=Ricerca
046402031126w   1C                   when      *inKd
046403031126     C                   exsr      search
046500020212e   1C                   endsl
046700020212      * Controllo dati immessi a video
046800020212     C                   exsr      CTRV2
047000020212      * Aggiornamento se non ci sono errori
047100020212if  1C                   if        not *in99
047200020212     C                             and (*inKF or *inKE or *inKQ)
047201031126     c                   if        not *in01
047202031126     c                   eval      tblkut = 1
047203031126     c                   eval      tblcod = 'XT'
047204031126     c                   eval      tblkey = v2cau
047205031126     c     ktab          chain     tabel00f
047206031126     c                   end
048201031126     C   kq              movel     '*'           TBlflg
048202031126     C                   exsr      aggio
048203031117     c                   leave
048300020212e   1C                   endif
048301031117     c                   enddo
048400020212      *
048500031117     C                   ENDSR
048700020212      *---------------------------------------------------------------*
048800020212      * CARV2  - Caricamento dati seconda videata                     *
048900020212      *---------------------------------------------------------------*
049000020212     C     CARV2         BEGSR
049100020212      *
049101031126     C                   Z-ADD     4             h1riga
049102031126     C                   Z-ADD     30            h1colo
049200020212     C                   clear                   T1opz
049201031126     C                   clear                   tabelds
049300031117     C                   movea     '00'          *in(01)
049400020212      *
049500020212      * Aggancio la tabella, se trovo il codice sono in modifica
049600020212      * o ripristino (se record annullato), altrimenti in immissione
049701031126     C                   movel     'XT'          TBlcod
049702031126     C                   movel(p)  V1cau         TBlkey
049704031126     c                   eval      tblkut = 1
049707031126     C     Ktab          chain     Tabel00f
049900031126if  1C                   if        %found(Tabel00f)
050100020212      * MODIFICA/RIPRISTINO
050200031126     C                   movel     TBluni        Dsxt
050300031126if  2C                   if        TBlflg = *blanks
050301031118     C                   setoff                                       02
050500020212     C                   eval      T1opz  = Opz(02)
050600020212x   2C                   else
050700031118     C                   seton                                        02
050800031118     C                   eval      T1opz  = Opz(05)
050900020212e   2C                   endif
051100020212x   1C                   else
051300020212      * IMMISSIONE
051400031126     c                   clear                   Dsxt
051500031117     C                   seton                                        01
051600020212     C                   eval      T1opz  = Opz(01)
051800020212e   1C                   endif
051900020212      *
052000031117     C                   move      V1cau         V2cau
052001031126     c                   clear                   v2des
052002031126     c                   clear                   ccd
052003031126     c                   clear                   des
052004031118     c                   if        not *in01
052200031126     C                   eval      v2des = §xtdes
052640031126     c                   movea     equi          ccd
052660031126     c* decodifico
052661031126     c                   do        20            xx
052662031126     c                   if        ccd(xx) <> ' '
052663031126     c                   movel     ccd(xx)       tblkey
052664031126     c                   exsr      srxu
052667031126     c                   end
054300031126     c                   enddo
054301031126     c                   end
054400020212     C                   ENDSR
055600020212      *---------------------------------------------------------------*
055700020212      * CTRV2  - Controllo e decodifica seconda videata               *
055800020212      *---------------------------------------------------------------*
055900020212     C     CTRV2         BEGSR
056000020212      *
056001031126     c                   movea     com20         *in(60)
056002031126     c                   setoff                                       525699
056200020212     C                   clear                   V2msg
056201031117     c                   do
056400020212      * Non si fanno i controlli se richisto l'annullamento
056500031117     C                   if        not *inkq
058400020212      * Descrizione obbligatoria
058500031117w   1C                   If        V2des =  *blanks
058600020913     C                   seton                                        52  99
058700031117     C                   movel     Msg(02)       V2msg
058800031117     C                   leave
058900020913     c                   endif
061301031126      * Passo i codici equipaggiamento in schiera
061334031126     c                   movea     ccd           equipag
061335031126     c* equipaggiamenti
061336031126     c                   if        equipag = ' '
061337031117     C                   seton                                        56  99
061338031126     C                   movel     Msg(04)       V2msg
061339031117     C                   leave
061340031126     c                   else
061341031126     c* decodifico
061342031126     c                   do        20            xx
061343031126     c                   if        ccd(xx) = ' '
061344031126     c                   clear                   des(xx)
061347031126     c                   else
061348031126     c                   movel     ccd(xx)       tblkey
061349031126     c                   exsr      srxu
061350031126     c                   end
061351031126     c                   enddo
061352031117     c                   end
061360031118     c                   end
061361031117     c                   enddo
069300020212      *
069400031117     C                   ENDSR
081800020212      *---------------------------------------------------------------*
081900031126      * AGGIO  * Aggiornamento tabella                                *
082000020212      *---------------------------------------------------------------*
082100031126     C     AGGIO         BEGSR
083400020212      *   Immissione
083500031118w   2C                   if        *in01
083600020212     C                   exsr      RIETBE
083900031126     C                   WRITE     Tabel
084000031117      *   Modifica
084100031118w   2C                   else
084101031126     c
084102031126     c                   if        tblflg <>'*'
084200020212     C                   exsr      RIETBE
084201031117     c                   end
085000031126     C                   UPDATE    Tabel
085200031118e   1C                   end
085400020212      * Torno alla prima videata che carico come da inizio pgm
085500020212     C                   movel     '1'           TipVid
086000020212     C                   ENDSR
086200020212      *---------------------------------------------------------------*
086300020212      * Riempimento file tabella
086400020212      *---------------------------------------------------------------*
086500020212     C     RIETBE        BEGSR
086600020212      *
086700031126     C                   clear                   Tabelds
086701031126     c                   z-add     1             tblkut
087400031126     C                   movel     'XT'          TBlcod
087500031126     C                   movel     V2cau         TBlkey
087800031126     C                   clear                   Dsxt
087900031126     C                   eval      §xtdes  =  V2des
088300031126     C                   eval      equi = ccd
090100031126     C                   movel(p)  Dsxt          TBluni
090600031126     C*
090700020212     C                   ENDSR
090702031118      *---------------------------------------------------------------*
090703031126     C*  Eseguo posizionamento su XU
090704031118      *---------------------------------------------------------------*
090705031126     C     SRxu          BEGSR
090706031126     c                   clear                   dsxu
090707031126     c                   eval      tblkut = 1
090708031126     c                   eval      tblcod = 'XU'
090710031126     c     ktab          chain     tabel00f
090715031126     c                   if        %found(tabel00f) and tblflg = ' '
090716031126     c                   movel     tbluni        dsxu
090717031126     c                   movel     §xudes        des(xx)
090718031118     C                   ELSE
090719031126     C                   eval      yy = 59 + xx
090720031126     c                   move      '1'           *in(yy)
090721031126     c                   seton                                        99
090722031126     C                   movel     Msg(03)       V2msg
090723031118     c                   end
090724031118     C                   ENDSR
090725031118      *---------------------------------------------------------------*
090726031118      * Operazioni Iniziali                                           *
090727031118      *---------------------------------------------------------------*
090728031118     C     *inzsr        BEGSR
090729031118      *
090730031118      * Ricezione parametri
090731031118     C     *entry        plist
090732031118     C                   parm                    KPJBA
090733031118      *
090734031118      * Definizioni chiavi di accesso
090735031126     C     Ktab          klist
090736031126     C                   kfld                    TBlkut
090737031126     C                   kfld                    TBlcod
090738031126     C                   kfld                    TBlkey
090742031118      *
090743031118      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
090744031118     C     *dtaara       define    §azute        azuteds
090745031118     C     *dtaara       define    §datiute      ddatiute
090746031118      *
090747031118     C                   clear                   AzUteDs
090748031118     C                   clear                   DDatiUte
090749031118     C                   clear                   Tibs34Ds
090750031118     C                   in(E)     *dtaara
090751031118if  1C                   IF        %Error  or  RSUT = *blanks
090752031118     C                   call      'TIBS34R'
090753031118     C                   parm                    Tibs34Ds
090754031118     C                   in        *dtaara
090755031118e   1C                   ENDIF
090756031118w   1C                   if        DUTERR = 'E'
090757031118     c                   exsr      fine
090758031118     c                   end
090759031118
090769031118     C                   eval      TipVid = '1'
090770031126      *  Definisco variabili
090771031126     C                   Z-ADD     0             CURR              2 0
090772031126     C                   Z-ADD     0             CURC              2 0
090773031118
090774031118     c                   EndSr
090800020212
090900020212** -OPZ-      *
091000020212  Inserimento
091100020212    Modifica
091300020212  Annullamento
091400020212Visualizzazione
091500020212   ANNULLATO
091600020212** -MSG-                                                                     *
091900031117Immettere la causale                                                           0
092000031117Immettere la descrizione                                                       0
092300031117Immettere un codice tabella corretto                                           0
092900031126Immettere almeno un opzional                                                   0
