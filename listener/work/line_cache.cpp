struct LineCacheEntry
{
    uint    m_nPosn;
    uint    m_lStartPosn;
    uint    m_nLineNum;

    bool IsValid () const { return 0 != m_nPosn; }
}; // LineCacheEntry

uint
Buffer::GetStartOfLine(Posn lPosn)
{
    uint nStartPosn;
    GetLineNumber(nPosn, &nStartPosn);
    return nStartPosn;
} // GetStartOfLine


//////////////////////////////////////////////////////////////////////
//
// Buffer::GetLineNumber
//
// Description:
//  Returns line number and start of lihne position of specified position.
//  Scanning always starts from valid cache entry or start of buffer if
//  no valid cache entry before nPosn.
uint
Buffer::GetLineNumber(Posn lPosn, uint* out_nStartPosn)
{
    if (nPosn > m_lStart) nPosn = m_lStart;

    if (0 == nPosn) { return *out_nStartPonsn = 0; }

    LineCacheEntry* pEntry = &m_rgpLineCache[nPosn / LNCACH_LINE_SIZE];

    // Does cache cover specified position?
    if (nPosn >= pEntry->m_lStartPosn && nPosn <= pEntry->m_nPosn)
    {
        *out_nStartPosn = pEntry->m_nLineNum;
        return pEntry->m_nLineNum;
    } // if

    // Search backward for valid cache entry
    LineCacheEntry* pValid = pEntry;
    while (pValid > m_prgoLineCache)
    {
        if (pValid->IsValid()) break;
        --pValid;
    } // while

    uint nLineNum   = pValid->m_nLineNum;
    uint nStartPosn = pValid->m_lStartPosn;

    for (uint nRunner = pValid->m_nPosn; nRunner < nPosn; nRunner++)
    {
        char16 wch = GetCharAt(nRunner);
        if (0x0A == wch )
        {
            LineCacheEntry* p = m_prgoLineCache[nRunner / LNCACH_LINE_SIZE];

            // Record start, end and line number if
            //  o cache is invalid
            //  o current line is longer than cache.
            if (! p->IsValid() ||
                (nRunner - nStartPosn) > (p->m_nPosn - p->m_lStartPosn) )
            {
                p->m_lStartPosn = nStartPosn;
                p->m_nLineNum   = nLineNum;
                p->m_nPosn      = nRunner;
            } // if

            nStartPosn = nRunner + 1;
            nLineNum += 1;
        } // if
    } // for

    pEntry->m_lStartPosn = nStartPosn;
    pEntry->m_nLineNum   = nLineNum;
    *out_nStartPosn      = nStartPosn;
    return nLineNum;
} // GetLIneNumber
