000100960119     H DECEDIT('0,') DATEDIT(*DMY.)
000200960119     H* FNLSN0R *----------------------------------------------------*
000300960119     H*-----------------*                                            *
000400050825     H*     - REPERIMENTO DATI DESTINATARIO FINALE -                 *
000500960119     H*                                                              *
000600960119      *--------------------------------------------------------------*
000700050825     FFNlbl02L  IF   E           K DISK
000701050825     FFNblp01L  IF   E           K DISK    usropn
000702050825     FFNarb01L  IF   E           K DISK    usropn
000704060918     Ffiar401L  IF   E           K DISK    usropn
000705050826     F*itas30c  IF   E           K DISK    usropn
000800960119      *--------------------------------------------------------------*
000900960119      *   D s                                                        *
001000960119      *--------------------------------------------------------------*
001001060918     D FNLblds       E DS                  extname(fnlbl00f)
001100050825     D FNLSN0ds      E DS
001200960119     D KPJBA         E DS
001900960119      *--------------------------------------------------------------*
002000960119      *   Ciclo Principale                                           *
002100960119      *--------------------------------------------------------------*
002200960119      *  Abblenco dati output
002300960119     C                   EXSR      AZZER
002301050829      *  Controllo se sede o filiale
002302050829     C*    DN0TRC        IFne      *BLANKS
002303050829     c                   exsr      srfil
002304050829     C*                  ELSE
002305050829     c*                  exsr      srsede
002306050829     c*                  end
002307050829     c* se dirottamento leggo il record dei legami altrimenti esco
002308050829     c                   if        cca <>'1'
002309050829     c                   eval      DN0TRV = 'N'
002310050829     c                   else
002312050829     c*
002313050825     c     klbl          setll     fnlbl02l
002314050825     c                   do        *hival
002315050825     c     klbl          reade     fnlbl02l
002316050825     c                   if        %eof(fnlbl02l)
002317060918     c                   if        dn0trv = ' '
002318050825     c                   eval      DN0TRV = 'N'
002319060918     c                   end
002320050825     c                   leave
002321050825     c                   else
002322050829     c
002400050825      *  Controllo se sede o filiale
002500050826     C*    DN0TRC        IFne      *BLANKS
002501050825     c                   exsr      srfil
002502060918     c                   if        dn0trv = 'S'
002503060918     c                   leave
002504060918     c                   end
003200050826     C*                  ELSE
003201050826     c*                  exsr      srsede
003202050826     c*                  end
003203050830     c                   end
003204050829     c* se dirottamento leggo il record dei legami successivo
003205050831     c                   if        cca = '1'
003208050825     c                   eval      kaas = LBLAAN
003209050825     c                   eval      klnp = LBLLPN
003210050825     c                   eval      knrs = LBLNRN
003211050825     c                   eval      knsp = LBLNSN
003212050825     c     klbl          setll     fnlbl02l
003214050825     c                   end
003215050825     c                   enddo
003216050829     c                   end
005300050825     C                   MOVEL     FNLSN0ds      KPJBU
005400960119     C                   SETON                                        LR
024000960119      *--------------------------------------------------------------*
024100050825      *   AZZER: inizilizzo tutti i campi di output                  *
024200960119      *--------------------------------------------------------------*
024300960119     C     AZZER         BEGSR
024400960119      *  Abblenco dati output
024500960119     C                   MOVEL     *BLANKS       DN0TRV
024600960119     C                   MOVEL     *BLANKS       DN0ERR
024700960119     C                   MOVEL     *BLANKS       DN0RSD
024800960119     C                   MOVEL     *BLANKS       DN0RS2
024900960119     C                   MOVEL     *BLANKS       DN0IND
025000960119     C                   MOVEL     *BLANKS       DN0LOD
025100960119     C                   MOVEL     *BLANKS       DN0PRD
025200960119     C                   MOVEL     *BLANKS       DN0NZD
025300960119     C                   MOVEL     *BLANKS       DN0CAD
025301060918     c                   clear                   fnlblds
025302060918     c                   eval      LBLAAN=dn0aas
025303060918     c                   eval      LBLLPN=dn0lnp
025304060918     c                   eval      LBLNRN=dn0nrs
025305060918     c                   eval      LBLNSN=dn0nsp
025400960125      *  Se la data apertura è = 0 imposto 99999999 in WDTGIA
025500960125     C                   Z-ADD     DN0DAG        WDTGIA
025600960125     C     DN0DAG        IFEQ      0
025700960125     C                   Z-ADD     99999999      WDTGIA            8 0
025800960125     C                   END
025900960119      *
026000960119     C                   ENDSR
026100960119      *--------------------------------------------------------------*
026200050825      *   filiale                                                    *
026300960119      *--------------------------------------------------------------*
026400050825     C     srfil         BEGSR
026501050825     c* se codice mancata consegna sulla bolla è = dirottamento e la
026502050825     c* lna è diversa dalla mamma vuol dire che esiste un successivo
026503050825     c* dirottamento quindi prosegno nella catena
026504050825     c                   clear                   cca
026505050825     c     kspe          chain     fnarb01l
026506050825     c* se arb già pulito verifico blp
026507050825     c                   if        not %found(fnarb01l)
026508050825     c     kspe          chain     fnblp01l
026509050825     c                   if        not %found(fnblp01l)
026510050825     c                   eval      dn0err = '1'
026511050825     c                   else
026512050830     c                   if        blpcca = ' '  and
026513050830     c                             lbllap <> lbllan  and
026514050908     c                             wdtgia > (lblaan * 10000 + LBLMGN)
026516060918     c                   eval      DN0RSD = blpRSD
026517060918     c                   eval      DN0IND = blpIND
026518060918     c                   eval      DN0LOD = blpLOD
026519060918     c                   eval      DN0PRD = blpPRD
026520060918     c                   eval      DN0CAD = blpCAD
026521060918     c                   eval      DN0NZD = blpNZD
026522060918     c                   exsr      srar4
026525050825     c                   end
026526050825     c                   eval      cca = blpcca
026527050825     c                   end
026528050825     c                   else
026529050830     c                   if        arbcca = ' ' and
026530050830     c                             lbllap <> lbllan  and
026531050908     c                             wdtgia > (lblaan * 10000 + LBLMGN)
026532060918     c                   eval      DN0RSD = arbRSD
026533060918     c                   eval      DN0IND = arbIND
026534060918     c                   eval      DN0LOD = arbLOD
026535060918     c                   eval      DN0PRD = arbPRD
026536060918     c                   eval      DN0CAD = arbCAD
026537060918     c                   eval      DN0NZD = arbNZD
026538060918     c                   exsr      srar4
026543050825     c                   end
026544050825     c                   eval      cca = arbcca
026545050825     c                   end
026546050825      *
026547050825     C                   ENDSR
026548060918      *--------------------------------------------------------------*
026549060918      *   legge ar4 x reperire ragione sociale ecc.                  *
026550060918      *--------------------------------------------------------------*
026551060918     C     srar4         BEGSR
026552060918     c                   clear                   ar4not
026553060918     c     k4            chain     fiar401l
026555060918     c                   eval      DN0RS2 = ar4not
026562060918     c                   eval      DN0TRV = 'S'
026563060918     C                   ENDSR
026564050825      *--------------------------------------------------------------*
026565050825      *   sede                                                       *
026566050825      *--------------------------------------------------------------*
026567050825     C     srsede        BEGSR
026568050825     c* se codice mancata consegna sulla bolla è = dirottamento e la
026569050825     c* lna è diversa dalla mamma vuol dire che esiste un successivo
026570050825     c* dirottamento quindi prosegno nella catena
026571050825     c                   clear                   cca
026572050826     c*    kspe          chain     titas30c
026573050825     c* se arb già pulito verifico blp
026574050826     c*                  if        not %found(titas30c)
026575050826     c*                  eval      dn0err = '1'
026576050826     c*                  else
026577050826     c*                  if        tascca = ' '
026578050826     c*                  eval      DN0RSD = tasRSD
026579050826     c*                  eval      DN0IND = tasIND
026580050826     c*                  eval      DN0LOD = tasLOD
026581050826     c*                  eval      DN0PRD = tasPRD
026582050826     c*                  eval      DN0CAD = tasCAD
026583050826     c*                  eval      DN0NZD = tasNZD
026584050826     c*                  eval      DN0TRV = 'S'
026585050826     c*                  end
026586050826     c*                  eval      cca = tascca
026587050826     c*                  end
026588050825      *
026589050825     C                   ENDSR
026590050825      *--------------------------------------------------------------*
026591050825      *   Operazioni iniziali                                        *
026592050825      *--------------------------------------------------------------*
026593050825     C     *INZSR        BEGSR
026594050825      *
026600960119     C     *ENTRY        PLIST
026700960119     C                   PARM                    KPJBA
026800050825     C                   MOVEL     KPJBU         FNLSN0ds
026801050825     c                   eval      kaas = dn0aas
026802050825     c                   eval      klnp = dn0lnp
026803050825     c                   eval      knrs = dn0nrs
026804050825     c                   eval      knsp = dn0nsp
026809050825     c* apro i file a seconda che sia sede o filiale
026810050826     C*    DN0TRC        IFne      *BLANKS
026811050825     c                   open      fnblp01l
026812050825     c                   open      fnarb01l
026814060918     c                   open      fiar401l
026815050826     C*                  ELSE
026816050826     c*                  open      titas30c
026817050826     c*                  end
026900960119      *  Definisco chiavi accesso
027000050825     C     Klbl          KLIST
027100960119     C                   KFLD                    KAAS
027200960119     C                   KFLD                    KLNP
027300960119     C                   KFLD                    KNRS
027400960119     C                   KFLD                    KNSP
027401050825     C     Kspe          KLIST
027402050825     C                   KFLD                    LBLAAN
027403050825     C                   KFLD                    LBLLPN
027404050825     C                   KFLD                    LBLNRN
027405050825     C                   KFLD                    LBLNSN
027406050829     C     K4            KLIST
027407050829     C                   KFLD                    LBLAAN
027408050829     C                   KFLD                    LBLLPN
027409050829     C                   KFLD                    LBLNRN
027410050829     C                   KFLD                    LBLNSN
027411050829     C                   KFLD                    trc
027412050829     c                   eval      trc = 'D'
027600960119      *  Definisco variabili
027700050825     C     *LIKE         DEFINE    BLpAAS        KAAS
027800050825     C     *LIKE         DEFINE    BLpLNP        KLNP
027900050825     C     *LIKE         DEFINE    BLpNRS        KNRS
028000050825     C     *LIKE         DEFINE    BLpNSP        KNSP
028001050825     C     *LIKE         DEFINE    BLpcca        cca
028002050829     C     *LIKE         DEFINE    ar4trc        trc
028200960119      *
028300960119     C                   ENDSR
