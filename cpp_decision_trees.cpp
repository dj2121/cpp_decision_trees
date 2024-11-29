#include <iostream>
#include<iomanip>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>
#include <algorithm>
#include <cmath>


using namespace std;

struct attributes{
	int row;
	string value;
};

struct node;

struct edge{
	node * ptr;
	string value;
};

struct node {
	int row;
	float entropy;
	vector<edge> child;
	int result;
};




vector<string> getdomain(int row, vector<vector<string> > &database){
	vector<string> domain;
	int include = 1;
	for(int i = 0; i < database.size(); i++){
		include = 1;
		for(int j = 0; j < domain.size(); j++){
			if(database[i][row]==domain[j]){
				include = 0;
			}
		}
		if(include==1){
			domain.push_back(database[i][row]);
		}
	}

	return domain;

}


float getcount(int row, vector<attributes> adata, vector<vector<string> > &database){
	
	float count = 0;
	int include = 1;

	for(int i = 0; i < database.size(); i++){
		include = 1;
		for(int j = 0; j < adata.size(); j++){
			if(database[i][adata[j].row]==adata[j].value){
			}
			else{
				include = 0;
			}
		}
		if(include==1){
			count++;
		}
	}
	return count;
}


float getentropy(vector<attributes> adata, vector<vector<string> > &database){

	float positive = 0, negative = 0, total = 0;
	int consider;

	for(int i = 0; i < database.size(); i++){
		consider = 1;
		for(int j = 0; j < adata.size(); j++){
			if(database[i][adata[j].row]==adata[j].value){
				continue;
			}
			else{
				consider = 0;
				break;
			}
		}

		if(consider==1){
			total++;
			if(database[i][3]=="yes"){
				positive++;
			}
			else{
				negative++;
			}
		}
		else{
		}
		
	}

	float ratio;

	if(positive==total || negative == total){
		ratio = 0;
	}
	else{
		float r1 = positive/total;
		float r2 = negative/total;
		ratio = -(r1 * log2(r1)) - (r2 * log2(r2));
	}
	return ratio;

}

float getgain(int row, float es, vector<string> domain, vector<attributes> adata, vector<vector<string> > &database){
	
	float gain = es;
	float total = getcount(row, adata, database);

	for(int i = 0; i < domain.size(); i++){
		float tempcount = 0, et;
		vector<attributes> adata2 = adata;
		attributes tatt;
		tatt.row = row;
		tatt.value = domain[i];
		adata2.push_back(tatt);
		tempcount = getcount(row, adata2, database);
		et = getentropy(adata2, database);

		float ratio = tempcount/total;

		float ev = ratio * et;
		gain = gain - ev;
	}

	return gain;

}

vector<vector<string> > getdata(string filename, int size){
    ifstream f1;
    char const *fname = filename.data();
    f1.open(fname, ios::in);
	string line, word, tt;

	int i = 0, j = 0;

	vector<vector<string> > data;

	vector<string> temp;

	if (f1.is_open()){
		while (!f1.eof()){
			temp.clear();
			j = 0;
			getline (f1, line);
			stringstream s(line); 
			while (getline(s, word, ',') && i<size) {
				temp.push_back(word);
				j++;
			}
			if(i > 0 && i < size){
				data.push_back(temp);
			}
			i++;
		}
		f1.close();
	}
	return data;
}

int getfilesize(string filename){
    fstream f1;
    char const *fname = filename.data();
    f1.open(fname, fstream::in | fstream::out);

    int i = 0;
    string line;

    if (f1.is_open()){
        while (!f1.eof() ){
            i++;
            getline (f1, line);
        }
        f1.close();
    }

    return i;
}

int getpcount(vector<attributes> adata, vector<vector<string> > &database){
	float count = 0;
	int include = 1;

	for(int i = 0; i < database.size(); i++){
		include = 1;
		for(int j = 0; j < adata.size(); j++){
			if(database[i][adata[j].row]==adata[j].value){
			}
			else{
				include = 0;
			}
		}
		if(include==1){
			if(database[i][3]=="yes"){
				count++;
			}
		}
	}
	return count;
}

int getmcount(vector<attributes> adata, vector<vector<string> > &database){
	float count = 0;
	int include = 1;

	for(int i = 0; i < database.size(); i++){
		include = 1;
		for(int j = 0; j < adata.size(); j++){
			if(database[i][adata[j].row]==adata[j].value){
			}
			else{
				include = 0;
			}
		}
		if(include==1){
			if(database[i][3]=="no"){
				count++;
			}
		}
	}
	return count;
}

node* buildtree(node *root, vector<attributes> adata, vector<vector<string> > &database, vector<vector<string> > &d){
	
	node *n = root;
	int row = n->row;
	
	float et = getentropy(adata, database);

	vector<edge> child;
	int include = 1;

	for(int i = 0; i < d[row].size(); i++){
		include = 1;
		for(int j = 0; j < adata.size(); j++){
			include = 1;
			if(adata[j].value==d[row][i]){
				include = 0;
			}
		}

		if(include==1){
			attributes atemp;
			atemp.row = row;
			atemp.value = d[row][i];
			vector<attributes> adata2 = adata;
			adata2.push_back(atemp);

			float et2 = getentropy(adata2, database);

			float g[3] = {0, 0, 0};
			float maxgain = 0;
			int maxi=0;
			int include2 = 1;
			for(int j = 0; j < 3; j++){
				include2 = 1;
					for(int k = 0; k < adata2.size(); k++){
					if(adata2[k].row == j){
						include2 = 0;
					}
				}

				if(include2==1){
					float gain = getgain(j, et2, d[j], adata2, database);
					if(gain>maxgain){
						maxgain = gain;
						maxi = j;
					}
				}
			}

			if(maxgain==0 || et2==0){
				int plus = getpcount(adata2, database);
				int minus = getmcount(adata2, database);
				vector<edge> ichild;
				edge iedge;
				iedge.ptr = NULL;
				iedge.value = "";
				ichild.push_back(iedge);
				node *tnode = new node();
				tnode->row = -1;
				tnode->entropy = 0;
				tnode->result = -1;
				tnode->child = ichild;
				if(plus>=minus){
					tnode->result = 1;
				}

				else{
					tnode->result = 0;
				}

				edge iedge2;
				iedge2.ptr = tnode;
				iedge2.value = d[row][i];
				child.push_back(iedge2);

			}

			else{
				vector<edge> ichild;
				edge iedge;
				iedge.ptr = NULL;
				iedge.value = "";
				ichild.push_back(iedge);

				node *tnode = new node();
				tnode->row = maxi;
				tnode->entropy = maxgain;
				tnode->result = -1;
				tnode->child = ichild;

				node *nt = buildtree(tnode, adata2, database, d);
				edge iedge2;
				iedge2.ptr = nt;
				iedge2.value = d[row][i];

				child.push_back(iedge2);
			}				
		}
	}

	n->child = child;
	return n;

}

void inorder(node *root, int n, string path){

	if(root==NULL) return;

	string db[3];
	db[0] = "pclass";
	db[1] = "age";
	db[2] = "gender";

	for(int i = 0; i < n; i++){
		cout << "\t";
	}

	if(path!="none"){
		cout <<"Edge: \"" << path << "\", ";
	}

	if(root->row >= 0){
		cout << "Attribute: " << db[root->row] << ", ";
	}

	cout <<  "Output: ";

	if(root->result==-1){
		cout << "None" << endl;
	}

	if(root->result>=0){
		cout << root->result << endl;
	}

	for(int i = 0; i < root->child.size(); i++){
		inorder(root->child[i].ptr, n+1, root->child[i].value);
	}

}

bool search(node *root, vector<string> input){
	node *n = root;
	while(n->row!=-1){
		for(int i = 0; i < n->child.size(); i++){
			if(input[n->row]==n->child[i].value){
				n = n->child[i].ptr;
				break;
			}
		}
	}

	return n->result;
}


int main(){

	cout << endl;
	cout << "Program here takes the same file for Training Desicion tree and Testing. Filename: data1_19.csv" << endl;
	cout << "You can supply seperate test file by changing testfilename string in main() to your test file name (with extension)." << endl;
	cout << "Gain will be defined as 0 when no further slicing was possible, in leaves." << endl << endl;
	cout << "Don't keep the dataset files open in another program while executing this." << endl << endl;
	cout << "Tree Output: 0 = death/no, 1 = survived/yes" << endl << endl;


    string filename = "data1_19.csv";

    int size = getfilesize(filename);
    size = size - 1;

    vector<vector<string> > data;
	data = getdata(filename, size);


	//Change testfilename if you want to supply seperate file for Testing
	string testfilename = "data1_19.csv";
	
    int size2 = getfilesize(testfilename);
    size2 = size2 - 1;

    vector<vector<string> > data2;
	data2 = getdata(testfilename, size2);

	vector<attributes> at;
	vector<vector<string> > d;

	d.push_back(getdomain(0, data));
	d.push_back(getdomain(1, data));
	d.push_back(getdomain(2, data));

	float g[3];

	float es;
	es = getentropy(at, data);

	float maxgain = 0;
	int maxi = 0;

	for(int i = 0; i < 3; i++){
		g[i] = getgain(i, es, d[i], at, data);
		if(g[i] > maxgain){
			maxi = i;
			maxgain = g[i];
		}
	}

	vector<edge> ichild;
	edge iedge;
	iedge.ptr = NULL;
	iedge.value = "";
	ichild.push_back(iedge);

	node *tnode = new node();
	tnode->row = maxi;
	tnode->entropy = g[maxi];
	tnode->result = -1;
	tnode->child = ichild;

	node *root = buildtree(tnode, at, data, d);

	cout << "Tree: (levels are differentiated by tabs)" <<endl;

	inorder(root, 0, "none");
	cout << endl;

	float p = 0, total = size2-1;

	for(int i = 0; i < data2.size(); i++){
		vector<string> temp;
		for(int j = 0; j < 3; j++){
			temp.push_back(data2[i][j]);
		}

		if(search(root, temp)){
			if(data2[i][3]=="yes"){
				p++;
			}
		}
		else{
			if(data2[i][3]=="no"){
				p++;
			}
		}
	}

	cout << "Accuracy: " << (p/total)*100 << endl;


    return 0;
}
