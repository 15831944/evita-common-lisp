//////////////////////////////////////////////////////////////////////////////
//
// evcl - listener - edit interval
// listener/winapp/ed_interval.h
//
// Copyright (C) 1996-2007 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evcl3/mainline/compiler/cl/cl_defs.h#34 $
//
#if !defined(INCLUDE_edit_binary_tree_h)
#define INCLUDE_edit_binary_tree_h

namespace Edit
{

template<class Node_>
class BinaryTree
{
    public: enum Color
    {
        Color_Red,
        Color_Black,
    }; // Color

    public: class NodeBase
    {
        friend class BinaryTree;

        private:   Color    m_eColor;
        protected: Node_*       m_pLeft;
        protected: Node_*       m_pParent;
        protected: Node_*       m_pRight;

        public: NodeBase() :
            m_eColor(Color_Black),
            m_pLeft(NULL),
            m_pParent(NULL),
            m_pRight(NULL) {}

        public: Color  GetColor()  const { return m_eColor; }
        public: Node_* GetLeft()   const { return m_pLeft; }
        public: Node_* GetParent() const { return m_pParent; }
        public: Node_* GetRight()  const { return m_pRight; }
    }; // NodeBase

    private: Node_* m_pRoot;

    public: BinaryTree() :
        m_pRoot(NULL) {}

    // [B]
    public: void Balance(Node_*);

    // [G]
    public: Node_* GetRoot() const { return m_pRoot; }

    // [R]
    private: void rotateLeft(Node_*);
    private: void rotateRight(Node_*);

    // [S]
    public: Node_* SetRoot(Node_* pRoot) { return m_pRoot = pRoot; }
}; // BinaryTree

//////////////////////////////////////////////////////////////////////
//
// BinaryTree::Balance
//
//  Balance tree after insertion.
//
template<class Node_>
void BinaryTree<Node_>::Balance(Node_* pNode)
{
    Node_* pParent = pNode->GetParent();

    // Case 1: The new node pNode is at the root of the tree.
    if (pParent == NULL)
    {
        pNode->m_eColor = Color_Black;
        return;
    } // if

    // Case 2: The new node's parent P is black.
    if (pParent->GetColor() == Color_Black)
    {
        // Tree is still valid.
        return;
    } // if

    Node_* pGrandParent = pParent->GetParent();
    ASSERT(pGrandParent != NULL);

    Node_* pUncle = pParent == pGrandParent->GetLeft() ?
        pGrandParent->GetRight() :
        pGrandParent->GetLeft();

    // Case 3: If both the parent P and the unclde U are red.
    if (pUncle != NULL && pUncle->GetColor() == Color_Red)
    {
        pParent->m_eColor = Color_Black;
        pUncle->m_eColor = Color_Black;
        pGrandParent->m_eColor = Color_Red;
        Balance(pGrandParent);
        return;
    } // if

    // Case 4: The parent P is red but the uncle U is black
    if (pNode == pParent->GetRight() &&
        pParent == pGrandParent->GetLeft() )
    {
        rotateLeft(pParent);
        pNode = pNode->GetLeft();
        pParent = pNode->GetParent();
        pGrandParent = pParent->GetParent();
    }
    else if (pNode   == pParent->GetLeft() &&
             pParent == pGrandParent->GetRight() )
    {
        rotateLeft(pParent);
        pNode = pNode->GetRight();
        pParent = pNode->GetParent();
        pGrandParent = pParent->GetParent();
    } // if

    // Case 5: The parent P is red but uncle U is black
    pParent->m_eColor = Color_Black;
    pGrandParent->m_eColor = Color_Red;
    if (pNode == pParent->GetLeft() && pParent == pGrandParent->GetLeft())
    {
        rotateRight(pGrandParent);
    }
    else
    {
        ASSERT(pNode   == pParent->GetRight());
        ASSERT(pParent == pGrandParent->GetRight());
        rotateLeft(pGrandParent);
    } // if
} // BinaryTree:Balance


//////////////////////////////////////////////////////////////////////
//
// BinaryTree::rotateLeft
//
//      Q           P
//      /\          /\
//     P  C   <==  A  Q
//        /\          /\
//       A  B        B  C
//
//  ((A P B) Q C) <== (A P (B Q C))
//
template<class Node_> void
BinaryTree<Node_>::rotateLeft(Node_* pP)
{
    Node_* pQ = pP->GetRight();
    Node_* pB = pQ->GetLeft();

    Node_* pParent = pP->GetParent();
    if (pParent == NULL)
    {
        m_pRoot = pQ;
    }
    else
    {
        if (pParent->GetLeft() == pP)
        {
            pParent->m_pLeft = pQ;
        }
        else
        {
            pParent->m_pRight = pQ;
        }
    } // if

    pP->m_pRight = pB;
    pQ->m_pLeft  = pP;
} // BinaryTree::rotateLeft


//////////////////////////////////////////////////////////////////////
//
// BinaryTree::rotateLeft
//      Q           P
//      /\          /\
//     P  C   ==>  A  Q
//     /\             /\
//    A  B           B  C
//
//  ((A P B) Q C) ==> (A P (B Q C))
//
template<class Node_> void
BinaryTree<Node_>::rotateRight(Node_* pQ)
{
    Node_* pP = pQ->GetLeft();
    Node_* pB = pP->GetRight();

    Node_* pParent = pQ->GetParent();
    if (pParent == NULL)
    {
        m_pRoot = pP;
    }
    else
    {
        if (pParent->GetLeft() == pQ)
        {
            pParent->m_pLeft = pP;
        }
        else
        {
            pParent->m_pRight = pP;
        }
    } // if

    pP->m_pRight = pQ;
    pQ->m_pLeft  = pB;
} // BinaryTree::rotateRight

} // Edit

#endif //!defined(INCLUDE_edit_binary_tree_h)
