000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000     H*             - IMMISSIONE GRANDI MANUTENZIONI                 *
000600081105     FTRmz93D   CF   E             WORKSTN
000601081105     dtrmz93ds       e ds
000602070731     dkpjba          e ds
000603070731     d* Parametri x Controllo profilo utenti
000604070731     d TIBS34ds      e ds
000605070731     d AZUTEds       e ds                  extname(AZUTE00F)
000606070731     d DDatiUte      e ds
000607070919     d dtaeur          s               d   datfmt(*eur)
000608070919     d dtaiso          s               d
000609070920     d comval          s               d
000610070920     d comdate         s               d
000611070802     d isodal          s               d
000612070802     d isoal           s               d
000613081105     d cliente         s              7
000616070925     d com10           s             10
000617070925     d vdal            s               d
000618070925     d val             s               d
015000000000     C*
015401070731     C                   DO        *HIVAL
015500000000     C                   EXFMT     GA28D001
015501011130     C* FINE LAVORO
015502011130     C   KC              LEAVE
015503070802     C* controlli
015504070802     C                   EXSR      CTR01
015505070802     C   99              ITER
016200000000     C* NUOVA MANUTENZIONE
016201011130     C                   IF        *INKF
016202081105     c                   clear                   trmz93ds
016203070731     c                   move      vdal          dtaiso
016204081105     c                   move      dtaiso        D93DTADA
016205070731     c                   move      val           dtaiso
016206081105     c                   move      dtaiso        D93DTAA
016207081105     c                   eval      kpjbu = trmz93ds
016208081105     c                   call      'TRMZ94C'
016209070731     c                   parm                    kpjba
016210070731     C                   leave
016211011130     C                   END
016212070731     C                   ENDdo
019204011130     c*
019500000000     C                   SETON                                        LR
019501070802     C******************************************************
019502070802     C* CONTROLLI PRIMO VIDEO
019503070802     C******************************************************
019504070802     C     CTR01         BEGSR
019505070925     c                   setoff                                       990102
019506070802     c                   clear                   vmsg
019507070802     c                   do
019508070925     c* controllo date
019509070925     c                   if        vdalm = 0 or vdalm > 12
019510070925     c                   seton                                        0199
019511070925     c                   eval      vmsg = 'DAL errato o mancante'
019512070925     c                   leave
019513070925     c                   end
019514070925     c                   if        vdala = 0
019515070925     c                   seton                                        0199
019516070925     c                   eval      vmsg = 'DAL errato o mancante'
019517070925     c                   leave
019518070925     c                   end
019519070925     c*
019520070925     c                   if        valm = 0 or valm > 12
019521070925     c                   seton                                        0299
019522070925     c                   eval      vmsg = 'AL errato o mancante'
019523070925     c                   leave
019524070925     c                   end
019525070925     c                   if        vala = 0
019526070925     c                   seton                                        0299
019527070925     c                   eval      vmsg = 'AL errato o mancante'
019528070925     c                   leave
019529070925     c                   end
019530070925     c*
019531070925     c                   eval      com10=
019532070925     C                               %editc(vdala:'X')
019533070925     C                             + '-'
019534070925     C                             + %editc(vdalm:'X')
019535070925     C                             + '-01'
019536070925     c                   movel     com10         vdal
019537070925     c                   eval      com10=
019538070925     C                               %editc(vala:'X')
019539070925     C                             + '-'
019540070925     C                             + %editc(valm:'X')
019541070925     C                             + '-01'
019542070925     c                   move      com10         dtaiso
019543070925     c                   adddur    1:*m          dtaiso
019544070925     c                   subdur    1:*d          dtaiso
019546070925     c                   movel     dtaiso        val
019547070802     c                   if        vdal > val
019548070802     c                   eval      vmsg = 'Data DAL maggiore di data AL'
019549070925     c                   seton                                        0299
019550070802     c                   leave
019551070802     c                   end
019552070919     c* controllo data finale: non deve essere del mese in corso oppure
019553070919     c* se mese precedente al corrente deve essere dopo la prima quindicina
019554070919     c                   if        val <> *loval
019555070920     c                   move      val           dtaeur
019556070920     c                   move      dtaeur        comval
019557070920     c                   move      *date         dtaeur
019558070920     c                   move      dtaeur        comdate
019559070920     c*
019560070919     c                   extrct    val:*m        mm                2 0
019561070920     c                   extrct    val:*y        aa                4 0
019562070920     c                   move      mm            mmaa              6 0
019563070920     c                   movel     aa            mmaa
019564070920     c                   move      *month        mmaac             6 0
019565070920     c                   movel     *year         mmaac
019566070920     c                   move      *date         comdta            8 0
019567070920     c                   movel     01            comdta
019568070920     c                   move      comdta        dtaeur
019569070919     c                   subdur    1:*d          dtaeur
019570070920     c                   extrct    dtaeur:*m     mmc1              2 0
019571070920     c                   extrct    dtaeur:*y     aac1              4 0
019572070920     c                   move      mmc1          mmaac1            6 0
019573070920     c                   movel     aac1          mmaac1
019574070921     c                   if        comval > comdate or (comval<=comdate and
019575070920     c                             (mmaa = mmaac or
019576070920     c                             (mmaa = mmaac1 and *day < 15)))
019577070919     c                   eval      vmsg = 'Data AL superiore al limite consenti-
019578070919     c                             to'
019579070925     c                   seton                                        0299
019580070919     c                   leave
019581070919     c                   end
019582070919     c                   end
019623081105     c* controllo che esista almeno un movimento nel periodo.
019625070802     c                   clear                   ok                1
019626081105     c                   move      vdal          isodal
019627081105     c                   move      val           isoal
019628081105     c                   move      isodal        comdalc           8 0
019629081105     c                   move      isoal         comalc            8 0
019630081105     c                   move      comdalc       comdal            8
019631081105     c                   move      comalc        comal             8
019632070802     C/EXEC SQL
019633081105     C+ DECLARE B1 CURSOR FOR SELECT CLIENTE FROM agip WHERE DATA_00003
019634081105     C+ between :comdal and :comal
019635070802     C/END-EXEC
019636070802
019637070802     C/EXEC SQL
019638070802     C+ OPEN b1
019639070802     C/END-EXEC
019640070802
019641070802     C                   dou       *inlr
019642070802
019643070802     C/EXEC SQL
019644081105     C+ FETCH NEXT FROM b1 INTO :cliente
019645070802     C/END-EXEC
019646070802
019647070802     C                   select
019648070802     C                   when      sqlcod = 100
019649070802     c                   leave
019650070802     C                   when      sqlcod = 0
019651070802     c                   eval      ok = '1'
019652070802     c                   leave
019653081106     c                   other
019654081106     c                   seton                                        99
019655081106     c                   leave
019656070802     C                   endsl
019657070802     C*
019658070802     C                   enddo
019659070802
019660070802     C/EXEC SQL
019661070802     C+ CLOSE b1
019662070802     C/END-EXEC
019663070802     c* errore se non esiste nemmeno un movimento
019664070802     c                   if        ok = ' '
019665070925     c                   seton                                        990102
019666070802     c                   eval      vmsg = 'Nel periodo scelto non esistono movi-
019667070802     c                             menti di autovetture'
019668070802     c                   leave
019669070802     c                   end
019670070802     c*
019671070802     c                   enddo
019672070802     C                   ENDSR
050400011130     C*----------------------------------------------------*
050500011130     c     *inzsr        begsr
050600011130     C*----------------------------------------------------*
053200011130     C     *ENTRY        PLIST                                                  *
053300011130     C                   PARM                    KPJBA                          *
053301070731      * Reperisco dati job
053302070731     c                   exsr      DatiJob
054200011130     c                   endsr
054300070731      *---------------------------------------------------------------*
054400070731      * Reperimento Dati del job (Utente/Operativi)                   *
054500070731      *---------------------------------------------------------------*
054600070731     c     DatiJob       BEGSR
054700070731      *
054800070731     c     *dtaara       define    §azute        azuteds
054900070731     c     *dtaara       define    §datiute      ddatiute
055000070731      *
055100070731     c                   in(E)     *dtaara
055200070731     c                   IF        %ERROR or RSUT = *blanks
055300070731     c                   clear                   Tibs34Ds
055400070731     c                   call      'TIBS34R'
055500070731     c                   parm                    Tibs34Ds
055600070731     c                   in        *dtaara
055700070731     c                   ENDIF
055800070731      *
055900070731     c                   ENDSR
