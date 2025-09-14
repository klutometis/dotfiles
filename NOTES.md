## 2025-08-11 - Downloads Directory Reorganization  

### Decision: Move Downloads to ~/var/web with symlink compatibility

**Context**: The default ~/Downloads directory violates FHS principles and feels out of place in a well-organized home directory structure.

**Problem with ~/Downloads:**
- Capitalized and verbose (doesn't match bin/, etc/, var/ structure)
- Not FHS-compliant 
- Takes up prime real estate in home directory

**Alternatives considered:**
- `~/var/spool` - Technically correct (data awaiting processing) but semantically awkward
- `~/var/dl` - Obvious abbreviation but not descriptive  
- `~/var/in` - Clean "inbox" concept
- `~/var/recv` - For "received" files

**Why we chose ~/var/web:**
- Only 9 characters (vs 12 for ~/Downloads)
- Semantically accurate - files grabbed from the web
- Fits perfectly with existing FHS structure (bin/, etc/, var/, doc/, prg/)
- Clear indication of file source

**Implementation**: 
- Added task to `configure-system.sh` to create `~/var/web` and symlink `~/Downloads -> ~/var/web`
- Maintains compatibility with applications that hardcode ~/Downloads
- Clean organizational structure while preserving functionality

## 2025-08-07 - Environment Variables and Secrets Management

### Decision: Use git-crypt + set -a instead of pass show

**Context**: Considered using `pass show` directly in `.env-secrets` instead of storing encrypted values.

**Why we didn't use `pass show`:**
- GPG unlock timing: We unlock GPG lazily (first browser use), but `.zshenv` loads early
- Shell startup would hang waiting for GPG unlock
- Background processes/scripts can't prompt for GPG passphrase  
- Performance: Every new shell would call `pass` multiple times

**Why we chose set -a approach:**
- Cleaner than repetitive `export` statements
- Fast shell startup (no external calls)
- Works in all contexts (cron, scripts, etc.)
- git-crypt provides adequate security for this use case

**Implementation**: Used `set -a` / `set +a` in `.env-secrets` to automatically export all variable assignments without repetitive `export` keywords.

## 2025-01-11 - Web Search API Comparison for MCP Servers

### Decision: Comprehensive analysis of top web search APIs for AI applications

**Context**: Researched the best web search MCP servers and APIs for AI agent integration.

**Performance & Accuracy Rankings (SimpleQA Benchmark):**
1. **Linkup**: 91.0% (State-of-the-art)
2. **Exa**: 90.04% (Very close second) 
3. **Perplexity Sonar Pro**: 86%
4. **Perplexity Sonar**: 77%
5. **Tavily**: 73%

**Detailed Comparison:**

**🏆 Linkup**
- Highest accuracy on factual benchmarks
- API-first company (dedicated focus)
- Simple, predictable pricing ($5/1000 calls)
- Premium content partnerships
- Fixed-rate pricing model
- Deep search mode for complex multi-step tasks
- Fast response times (2-20 seconds depending on mode)
- **Best for:** Business intelligence, lead enrichment, factual research, cost-conscious developers

**🥈 Exa (formerly Metaphor)**
- Neural/semantic search (understands meaning, not just keywords)
- Very high accuracy (90.04%)
- Fast response times (~2-3 seconds)
- Good for finding semantically similar content
- Strong developer community adoption
- **Weaknesses:** Can struggle with multi-step complex tasks, more expensive for research-heavy workflows
- **Best for:** Semantic search, finding similar content, speed-critical applications

**🥉 Tavily**
- AI-optimized search specifically for agents
- Good balance of search + scraping capabilities
- Extract API for content scraping
- Modular approach (separate search/scrape endpoints)
- Easy LangChain integration
- **Weaknesses:** Lower accuracy compared to Linkup/Exa, more expensive than Linkup ($8/1000 calls)
- **Best for:** AI agents needing both search and scraping, LangChain workflows

**🔍 Perplexity Sonar**
- Real-time web access with generated answers and citations
- Good for conversational AI
- **Weaknesses:** Lower accuracy scores, complex variable pricing (65% more expensive than Linkup), performance "tuned down by design" to protect consumer product
- **Best for:** Conversational AI where you want pre-generated answers

**Cost Comparison (per 1000 calls):**
- Linkup: $5 (fixed rate)
- Exa: $5 (basic), higher for research mode
- Tavily: $8
- Perplexity Sonar: $9.44-$16.44 (variable)
- Perplexity Sonar Pro: $70.52-$78.52 (variable)

**Developer Feedback Summary:**
- Speed ranking: Exa > Tavily > Linkup
- Quality ranking: Linkup > Tavily > Exa
- Cost-effectiveness: Linkup consistently most affordable
- Reliability: Linkup praised for consistency

**Recommendation:** For most AI applications requiring web search, **Linkup appears to be the current leader** due to its combination of highest accuracy, lowest cost, and API-first focus.
