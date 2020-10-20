#include <iostream>
#include <stdio.h>
#include <vector>
#include <fstream>
#include <sstream>
#include <string.h>
#include<algorithm>
#include<set>
#include<fstream>
using namespace std;
class item_Sets
{
    public:
    vector<int> item;
    int count;
};
string str[34]={"handicapped-infantsY","handicapped-infantsN","water-project-cost-sharingY","water-project-cost-sharingN","adoption-of-the-budget-resolutionY","adoption-of-the-budget-resolutionN",
"physician-fee-freezeY","physician-fee-freezeN","el-salvador-aidY","el-salvador-aidN","religious-groups-in-schoolsY","religious-groups-in-schoolsN","anti-satellite-test-banY","anti-satellite-test-banN"
,"aid-to-nicaraguan-contrasY","aid-to-nicaraguan-contrasN","mx-missileY","mx-missileN","immigrationY","immigrationN","synfuels-corporation-cutbackY","synfuels-corporation-cutbackN",
"education-spendingY","education-spendingN","superfund-right-to-sueY","superfund-right-to-sueN","crimeY","crimeN","duty-free-exportsY","duty-free-exportsN"
 ,"export-administration-act-south-africaY","export-administration-act-south-africaN","democrat", "republican"};
float minsupport=0.4,minconfidence=0.8;
int data[435][17];
vector<int> F1;
vector <item_Sets> F[18],CF[18];
ofstream fo;
class HashTree
{
  public:
  HashTree *k[10];
  int count;
  vector <item_Sets *> hf;
  HashTree()
  {
      int i;
      for(i=0;i<10;i++)
        k[i]=NULL;
      count=0;
  }
};
HashTree headFreq[18];
void Frequent_Items()
{
    int i,j;
    float temp;
    int arr[35]={0};
    for(i=0;i<435;i++)
    {
        for(j=0;j<17;j++)
            {
                arr[data[i][j]]++;
            }
    }
    for(i=1;i<35;i++)
    {
        temp=(float(arr[i])/435.0);
        if(temp>minsupport)
        {
            item_Sets temp_set;
            F1.push_back(i);
            temp_set.item.push_back(i);
            temp_set.count=arr[i];
            F[1].push_back(temp_set);
        }
    }
}
void Candidate_Items(int k)
{
    int item,up,i;
    item_Sets temp_set;
    for(i=0;i<F[k-1].size();i++)
    {
        item=F[k-1][i].item[k-2];
        up=upper_bound(F1.begin(),F1.end(),item)-F1.begin();
        if(F1[up]!=item)
        {
            if(item%2==1 && F1[up]==item+1)
                up++;
            for(;up<F1.size();up++)
            {
                item_Sets temp_set;
                temp_set=F[k-1][i];
                temp_set.count=0;
                temp_set.item.push_back(F1[up]);
                CF[k].push_back(temp_set);
            }
        }
    }
}
void insert(item_Sets *is,HashTree *head,int lev,int k)
{
    int i=0;
    if(lev>=k)
    {
        head->hf.push_back(is);
        head->count++;
        return;
    }
    else if(head->count!=-1 && (head->count+1)<=10)
    {
        head->hf.push_back(is);
        head->count++;
    }
    else if(head->count==-1)
    {
                insert(is,head->k[is->item[lev]%10],lev+1,k);
    }
    else if((head->count+1)>10)
    {
        head->hf.push_back(is);
        for(i=0;i<10;i++)
        {
            head->k[i]=new HashTree();
            head->k[i]->count=0;
        }
        for(i=0;i<head->hf.size();i++)
        {
            insert(head->hf[i],head->k[head->hf[i]->item[lev]%10],lev+1,k);
        }
        head->count=-1;
        head->hf.clear();
    }
}
void traverse(HashTree *head,vector<int> li,int lev)
{
    if(head->count!=-1)
    {
        int i;
        for(i=0;i<head->count;i++)
        {
            if(li==head->hf[i]->item)
            {
                head->hf[i]->count++;
                return;
            }
        }
        return;
    }
    else
    {
        traverse(head->k[li[lev]%10],li,lev+1);
    }
}
float Left_Support(HashTree *head,vector<int> li,int lev)
{
    if(head->count!=-1)
    {
        int i;
        for(i=0;i<head->count;i++)
        {
            if(li==head->hf[i]->item)
            {
                return head->hf[i]->count/1.0;
            }
        }
        return 0.0;
    }
    else
    {
        Left_Support(head->k[li[lev]%10],li,lev+1);
    }
}
void Update_count(HashTree *head,int i,int k,int fis,vector<int> li)
{
    int j;
    if(k==0)
    {
        traverse(head,li,0);
        return;
    }
    for(;fis<=(17-k);fis++)
    {
        if(data[i][fis]!=0)
        {
            li.push_back(data[i][fis]);
            /*for(j=0;j<li.size();j++)
            {
                cout<<li[j]<<" ";
            }*/
            Update_count(head,i,k-1,fis+1,li);
            li.pop_back();
        }
    }
}

void Candidate_Pruning(int k)
{
    int i,j=0;
    HashTree head;
    vector<int> li;
    for(i=0;i<CF[k].size();i++)
    {
        insert(&CF[k][i],&head,0,k);
    }
    for(i=0;i<435;i++)
    {
            Update_count(&head,i,k,0,li);
    }
    for(i=0;i<CF[k].size();i++)
    {
        if((float (CF[k][i].count)/435.0)>minsupport)
        {
            F[k].push_back(CF[k][i]);
            //insert(&CF[k][i],&headFreq[k],0,k);
        }
    }
}
void All_Frequent(int k)
{
    int i,j;
    if(F[k-1].size()<k-1)
        return;
    Candidate_Items(k);
    Candidate_Pruning(k);
}
void Make_rule(vector <int> l1,vector<int> r1,int right,int i,int k)
{
    int j;
    vector<int> l2;
    if(right>=k-1)
    {
        float count=Left_Support(&headFreq[k-right],l1,0);
        if(F[k][i].count/count>minconfidence)
        {
            fo<<"{";
            for(j=0;j<l1.size();j++)
            {
                fo<<str[l1[j]-1]<<" ";
            }
            fo<<"}=>{";
            for(j=0;j<r1.size();j++)
            {
                fo<<str[r1[j]-1]<<" ";
            }
            fo<<"}";
            fo<<"\n";
            //gcount++;
            return;
        }
        else
        {
                return;
        }
    }
    else
    {
        if(right>0)
        {
            float count=Left_Support(&headFreq[k-right],l1,0);
            if(F[k][i].count/count>minconfidence)
            {
                fo<<"{";
                for(j=0;j<l1.size();j++)
                {
                        fo<<str[l1[j]-1]<<" ";
                }
                fo<<"}=>{";
                for(j=0;j<r1.size();j++)
                {
                        fo<<str[r1[j]-1]<<" ";
                }
                fo<<"}";
                fo<<"\n";
                //gcount++;
            }
            else
            {
                return ;
            }
        }
        for(j=0;j<l1.size();j++)
        {
            for(int r=0;r<l1.size();r++)
            {
                if(r==j)
                    continue;
                else
                    l2.push_back(l1[r]);
            }
            if(right==0 || l1[j]>r1[r1.size()-1])
            {
                r1.push_back(l1[j]);
                Make_rule(l2,r1,right+1,i,k);
                r1.pop_back();
            }
            l2.clear();
        }
    }
}
void Make_all_rules(int k)
{
    int i=0,j=0;
    vector <int> l1;
    vector <int> r1;
    for(i=0;i<F[k].size();i++)
    {
        l1=F[k][i].item;
        Make_rule(l1,r1,0,i,k);
    }
}
void encode()
{
	//int data[495][17];
	std::ifstream file("dataset.txt");
	string line;
	int j;
	//cout<<"hi \n";
	for(j=0;j<435;j++)
	{
	    int count=0;
	 	std::getline(file,line);
		std::stringstream linestream(line);
		string item;
		int i;
		for(i=0;i<17;i++)

		{

			std::getline(linestream,item,',');
			char *a = const_cast<char*>(item.c_str());

			if(strcmp(a,"'y'")==0)
			{
				data[j][i]=2*i+1;
				//cout<<data[j][i]<<"\n";
			}
			else if(strcmp(a,"'n'")==0)
			{
				data[j][i]=2*i+2;
				//cout<<data[j][i]<<"\n";
			}
			else if(strcmp(a,"'democrat'")==0)
			{
				data[j][i]=33;
				//cout<<data[j][i]<<"\n";
			}
			else if(strcmp(a,"'republican'")==0)
			{
				data[j][i]=34;
				//cout<<data[j][i]<<"\n";
			}
			else
			{
				data[j][i]=0;
				//cout<<data[j][i]<<"\n";
			}
        }
	}
}
int main()
{
    int i=0,j;
    encode();
    Frequent_Items();
    for(i=0;i<435;i++)
    {
    	for(j=0;j<17;j++)
    	{
    		printf("%d ",data[i][j]);
		}
		printf("\n\n");
	}
    for(i=2;i<=17;i++)
    {
        All_Frequent(i);
    }
    fo.open("rules.txt");
    for(i=2;i<=17;i++)
    {
        if(F[i].size()>0)
            Make_all_rules(i);
    }
    fo.close();
    cout<<"DONE";
    cin>>i;
}
